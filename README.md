# Costos-produccion

Autor: Hector Camargo
Email:hacamargoa@hotmail.com, hac809@stdent.bham.ac.uk
Estimación de costos de producción a nivel departamental para Colombia, los costos estiman los precios de insecticidas y fungicidas como promedio ponderado de los productos disponibles en cada departamento y de las ventas de ingredientes en Colombia. 

Forma de Uso
La herramienta se puede correr paso a paso en el siguiente orden:
-Correr el script Packages.R: Este archivo se debe correr solo la primera vez que se usa la App e instalará los paquetes necesarios para el funcionamiento.
Agricola
-inputs_ag.R:Compilación de todas las bases de datos ICA y SIPSA relacionadas con insumos y servicios para la estimación de costos.
-Tablas_agricola.R:Cálculo de los valores para insumos y jornales por línea por departamento basado en los inputs. Precios de enmiendas, fertilizantes coadyuvantes y foliares son calculados por departamento. Mientras que fungicidas, insecticidas y herbicidas son calculados por departamento y cultivo para el que fueron registrados.
-Costos_ag.R:Estima las tablas de costos por departamento y línea productiva y las almacena en la lista Deptos.

Pecuario

-inputs_pec.R: Compilación de todas las bases de datos ICA y SIPSA relacionadas con insumos y servicios para la estimación de costos.
-Tablas_pecuario.R: Cálculo de los valores para insumos y jornales por linea por departamento basado en los inputs. Precios de enmiendas, insecticidas, herbicidas, especies animales fueron calculados por departamento. Mientras que insumos como alimento, vacunas, medicinas, vitaminas también son filtrados por línea productiva.
-Costos_Pec: Estima las tablas de costos por departamento y línea productiva y las almacena en la lista DeptosV.

La forma mas rápida de obtener una interfase fácil es corriendo el script app.R allí se puede visualizar toda la información y mapas

Tambien se puede acceder online usando el link https://hacamal.shinyapps.io/costos-produccion/