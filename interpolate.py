import numpy as np
import pandas as pd
import os
import math
import time
# import tkinter as tk


class coordinates_3D:
	def __init__(self, x, y, z):
		self.x = x
		self.y = y
		self.z = z
	# def __del__(self):
	# 	del self.x, self.y, self.z

class coordinates_2D:
	def __init__(self, x, y):
		self.x = x
		self.y = y
	# def __del__(self):
	# 	del self.x, self.y

#------Reading values from file Function---------
def parse(filename):
	mat = np.loadtxt(filename)
	mat = np.transpose(mat)
	return mat

#Calculating the length of square sides Function	
def side(p1_2d, p2_2d):
	return math.sqrt(math.pow((p1_2d.x - p2_2d.x), 2) + math.pow((p1_2d.y - p2_2d.y), 2))


#--------------------------------------------------------------------------
#Interpolating function
def depth(xx, yy, Point1, Point2, Point3):
	#swap min() to Point number 1
	if Point2.z == min(Point1.z, Point2.z, Point3.z):
		tam = Point1
		Point1 = Point2
		Point2 = tam
	elif Point3.z == min(Point1.z, Point2.z, Point3.z):
		tam = Point3
		Point3 = Point1
		Point1 = tam
	#300 in fortran code
	if ( xx == Point1.x and xx == Point2.x ) or ( xx == Point1.x and xx == Point3.x ) or ( xx == Point2.x and xx == Point3.x ):
		if (Point1.x == Point2.x and Point1.y == Point2.y):
			if (Point1.z <= Point2.z):
				depth = Point1.z + ( yy - Point1.y ) * ( Point2.z - Point1.z ) / ( Point2.y - Point1.y )
			else:
				depth = Point2.z + ( yy - Point2.y ) * ( Point1.z - Point2.z ) / ( Point1.y - Point2.y )
		elif (Point2.x == Point3.x and Point2.y != Point3.y):
			if ( Point2.z <= Point3.z ):
				depth = Point2.z + ( yy - Point2.y ) * (Point3.z - Point2.z) / ( Point3.y - Point2.y)
			else:
				depth = Point3.z + ( yy - Point3.y ) * (Point2.z - Point3.z) / ( Point2.y - Point3.y)
		elif (Point1.x == Point3.x and Point1.y != Point3.y):
			if (Point1.z <= Point3.z):
				depth = Point1.z + ( yy - Point1.y) * (Point3.z - Point1.z) / (Point3.y - Point1.y)
			else:
				depth = Point3.z + ( yy - Point3.y) * (Point1.z - Point3.z) / (Point1.y - Point3.y)
	#400 in fortran code
	if ( yy == Point1.y and yy == Point2.y ) or ( yy == Point1.y and yy == Point3.y ) or ( yy == Point2.y and yy == Point3.y ):
		if (Point1.y == Point2.y and Point1.x != Point2.x):
			if (Point1.z <= Point2.z):
				depth = Point1.z + (xx - Point1.x) * (Point2.z - Point1.z) / (Point2.x - Point1.x)
			else:
				depth = Point1.z + (xx - Point2.x) * (Point1.z - Point2.z) / (Point1.x - Point2.x)
		elif (Point2.y == Point3.y and Point2.x != Point3.x):
			if (Point2.z <= Point3.z):
				depth = Point2.z + (xx - Point2.x) * (Point3.z - Point2.z) / (Point3.x - Point2.x)
			else:
				depth = Point3.z + (xx - Point3.x) * (Point2.z - Point3.z) / (Point2.x - Point3.x)
		elif (Point1.y == Point3.y and Point1.x != Point3.x):
			if (Point1.z <= Point3.z):
				depth = Point1.z + (xx - Point1.x) * (Point3.z - Point1.z) / (Point3.x - Point1.x)
			else:
				depth = Point3.z + (xx - Point3.x) * (Point1.z - Point3.z) / (Point1.x - Point3.x)
	dz2 = Point2.z - Point1.z
	dz3 = Point3.z - Point1.z
	if (Point2.x != Point3.x ):
		a23 = ( Point2.y - Point3.y ) / ( Point2.x - Point3.x )
	b23 = Point2.y - a23 * Point2.x
	a1n = (Point1.y - yy ) / ( Point1.x - xx)
	b1n = Point1.y - a1n * Point1.x
	if ( Point2.x != Point3.x ):
		xm = ( b23 - b1n )/ ( a1n - a23)
	else:
		xm = Point2.x
	ym = a1n * xm + b1n
	point_m = coordinates_2D(xm, ym)
	point_xy = coordinates_2D(xx, yy)
	point1_2d = coordinates_2D(Point1.x, Point1.y)
	point2_2d = coordinates_2D(Point2.x, Point2.y)
	point3_2d = coordinates_2D(Point3.x, Point3.y)
	sm2 = side(point_m, point2_2d)
	sm3 = side(point_m, point3_2d)
	s23 = sm2 + sm3
	if ( dz3 > dz2 ):
		dzm = dz2 + sm2 * ( dz3 - dz2 ) / s23
	else:
		dzm = dz3 +sm3 * ( dz2 - dz2 ) / s23
	snm = side(point_xy, point_m)
	sn1 = side(point_xy, point1_2d)
	sm1 = snm + sn1
	depth = Point1.z + sn1 * dzm / sm1
	return depth
#------------------------------------------------------------------
# #######################################################
# ######### 		Tkinter					###############
# #######################################################

# class Application(tk.Frame):
#     def __init__(self, master=None):
#         super().__init__(master)
#         self.master = master
#         self.pack()
#         self.create_widgets()

#     def create_widgets(self):
#         self.hi_there = tk.Button(self)
#         self.hi_there["text"] = "Hello World\n(click me)"
#         self.hi_there["command"] = self.say_hi
#         self.hi_there.pack(side="top")

#         self.quit = tk.Button(self, text="QUIT", fg="red",
#                               command=self.master.destroy)
#         self.quit.pack(side="bottom")

#     def say_hi(self):
#         print("hi there, everyone!")
# #--------------------------------------------------------------#


# root = tk.Tk()
# app = Application(master=root)
# app.mainloop()










#################################################################
####			MAIN 										#####
#################################################################



def main():
	print("\n")
	print("--------------------------------------------------------")
	print("#######################################################")
	print("#####		INTERPOLATING PROGRAM		    ##")
	print("#######################################################")

	###################################
	start_time = time.time()

	#declare result and error files:
	f1 = open("result.txt","w")
	f2 = open("error.txt","w")

	#Reading values in input files
	
	data_arr = parse("data.txt")
	props_arr = parse("props.txt")
	#"Input interpolating point file name : ")
	intp_arr  = parse("intp.txt")
	#-----------------------------------------
	(Nx, Ny) = props_arr[:, 0]
	Nx = int(Nx)
	Ny = int(Ny)
	(xd, xc) = props_arr[:, 1]
	(yd, yc)= props_arr[:, 2]
	xnew = np.array(intp_arr[0,:])
	ynew = np.array(intp_arr[1,:])
	# (:, nnew ) = np.shape(intp_arr)
	nnew = intp_arr.shape[1]
	#-----------------------------------------
	NN = Nx * Ny
	dx = (xc - xd) / (Nx - 1)
	dy = (yc - yd) / (Ny - 1)
	#------------------------------------------
	x = np.array([xd + i * dx for j in range(Ny) for i in range(Nx)])
	y = np.array([yd + j * dy for j in range(Ny) for i in range(Nx)])
	z = np.array([data_arr[i, j] for j in range(Ny) for i in range(Nx)])
	# for j in range(Ny):
	# 	for i in range (Nx):
	# 		x[j * Nx + i] = xd + i * dx
	# 		y[j * Nx + i] = yd + j * dy
	# 		z[j * Nx + i] = data_arr[i, j] 

	print("#--------------------------------------------------#")
	print("We got " + str(NN) + " data points ; Number of interpolating points : " + str(nnew))
	print("#--------------------------------------------------#")

	
	###Interpolating Block
	k = 0
	ier = []
	for i in range(0, nnew):
		i1 = 0
		i2 = 0
		i3 = 0
		i4 = 0
		insuy = 0
		xx = xnew[i]
		yy = ynew[i]

		###--------------------------------------###
		# Defining rectangle contained interpolating point #
		for j in range(0, NN):
			ddx = xx - x[j]
			ddy = yy - y[j]
			if (ddx == 0) and (ddy == 0):
				zz = z[j]
				insuy =1
				break
			elif (abs(ddx) <= 0) and (abs(ddy) <= 0):
				if ddx >= 0:
					if ddy >= 0:
						i1 = j
					else:
						i4 = j
				else:
					if ddx >= 0:
						i2 = j
					else: 
						i3 = j
				break
		
		if (i1 == 0) and (i2 == 0) and (i3 == 0) and (i4 == 0):
			if insuy == 0:
				k += 1				# k points can't interpolate
				ier.append(i)		# ier[0 : k_final -1] is error point that can't interpolate
			else:
				f1.write(str(xnew[i]) + "	" + str(ynew[i]) + "	" + zz + "/n")
		else:
			# Defining triangle contained interpolating point #
			dd = x[i2] - xx
			dm = (y[i3] - yy) * dx / dy
			if dd > dm: i2 =i4
			Point1 = coordinates_3D(x[i1], y[i1], z[i1])
			Point2 = coordinates_3D(x[i2], y[i2], z[i2])
			Point3 = coordinates_3D(x[i3], y[i3], z[i3])
			zz = depth(xx, yy, Point1, Point2, Point3)
			f1.write(str(xnew[i]) + "	" + str(ynew[i]) + "	" + zz + "/n")
		rdo = float(i) / nnew * 100
		if i % 3 == 0:
			print(str(time.time() - start_time) + " seconds : " + " Running..." + str(rdo) + "  %" )
		elif i % 3 == 1:
			print(str(time.time() - start_time) + " seconds : " + " Running.  " + str(rdo) + "  %" )
		else:
			print(str(time.time() - start_time) + " seconds : " + " Running.. " + str(rdo) + "  %" )
					
	#Print results to files !
	f1.close()
	
	f2.write(" There are " + str(k) + " points can't be interpolated /n")
	for i in range(0, k):
		f2.write(str(xnew[i]) + "	" + str(ynew[i]) + "/n")
	f2.close()
	print("\n")
	print("-------------------------------------------------------")
	print(" Successfull Interpolating")
	print("Finished !!!")


if __name__ == "__main__":
     main()









