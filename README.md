# Legendre polynomial applying in animal breeding


#### ***Introduction for Legendre polynomials***
![image](https://user-images.githubusercontent.com/43666525/149734148-65e1e214-384a-4754-a2f5-da3705b36186.png)
###### The upsides and downsides of this method:

* 

##### Function 1: Calculating the matrix of Legendre polynomials (including the process for standardization)

```r
len_m <- lengder_stand(x,n)
x is the vector of environmental variable 
n is the order
```
##### Function 2: Calculating the standardized matrix for Legendre polynomials 

```r
tm <- time_matrix(t_min,t_max,t_interval,t_order)
t_min is the minimum of environmental variable
t_max is the maximum of environmental variable
t_interval is the interval of this variable
t_order is the order the Legendre polynomials 
```
##### Function 3: Calculating the results for Legendre polynomials and creat plot

```r
Legendre_plot(len_order_matrix,t_min,t_max,t_interval,t_order)
len_order_matrix is the result for each Legendre polynomials
The other parameters is the same to function 2
```
