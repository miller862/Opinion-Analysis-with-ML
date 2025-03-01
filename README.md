# Clasificación y Agrupamiento: Machine Learning aplicado a una encuesta política en Argentina

Este repositorio contiene el código y los datos utilizados en el Trabajo Final del curso **Sistemas de Información y Bases de Datos Estadísticos** de la Universidad Nacional de Tres de Febrero.

## 📌 Introducción
Distintas sociedades occidentales parecen atravesar un proceso a primera vista contradictorio que es materia de estudio relevante para la Ciencia Política: la difusión y creciente liquidez en el contenido de las identidades, al mismo tiempo que la radicalización y exacerbación de la violencia en el debate público en torno a estas etiquetas identitarias aumentan.

Este problema lleva a preguntarse cómo distintas ideas culturales y políticas se relacionan entre sí y cuál es su correspondencia con la identificación política de una persona en Argentina. Para ello, se diseñó una **encuesta de opinión política**, obteniendo una muestra no probabilística mediante Google Forms con un muestreo en bola de nieve.

Se analizaron **110 sujetos**, evaluando su posicionamiento frente a ciertos dilemas y frases del debate público argentino, su identificación ideológica y su opinión sobre otros países, además de datos demográficos y socioeconómicos.

---

## 📊 **Estructura del Repositorio**
Este repositorio contiene los siguientes archivos:

### **📁 Datos**
- `134NODUMMY.xlsx` → Variables en su nivel de medición original.  
  - 🔹 **Finalidad:** Análisis descriptivo.

- `D134.xlsx` → Variables categóricas transformadas en DUMMY, eliminando una modalidad para evitar colinealidad.  
  - 🔹 **Finalidad:** Modelos de clasificación.

- `kmeans134.xlsx` → Igual a `D134.xlsx` pero sin eliminar modalidades.  
  - 🔹 **Finalidad:** Modelos de clustering.

---

### **📜 Código**
- `FINAL_BASES.ipynb` → **Jupyter Notebook principal** con:
  - 🔹 **Carga y exploración de datos** en Python.
  - 🔹 **Aplicación de XGBoost** para clasificación de la identificación política.
  - 🔹 **Reducción de variables** eliminando aquellas con menor ganancia de información.
  - 🔹 **Detección de sesgos en el modelo** (ej. sobrerrepresentación de ciertas categorías).
  - 🔹 **Comparación con regresión logística** para mejorar la precisión.
  - 🔹 **Clustering con K-Means** para evaluar la congruencia entre autopercepción política y grupos formados automáticamente.

---

## 🔧 **Requisitos y Ejecución**
Para ejecutar este proyecto localmente, sigue los siguientes pasos:

### **1️⃣ Clonar el repositorio**
```bash
git clone https://github.com/tu_usuario/tu_repositorio.git
cd tu_repositorio
