---
title: "AxoMetric"
author: "Matthew Finneran"
output: html_fragment
---
# <center>**AxoMetric**</center>

Welcome to AxoMetric: An automatic interval-based quantification platform optimized for axon, mean fluorescence intensity, and cell body quantification. The purpose of this software is to create a tool for researchers to analyze tissue staining within distance intervals in an unbiased, consistent, and accessible manner. This platform will allow users to normalize tissue staining to tissue width based on cellular nuclear staining. This software was developed based on mouse optic and sciatic nerve longitudinal sections, however this may be applicable to other species / types of sections. The platform also includes a tool to measure the number of retinal ganglion cells (RGCs) in retina flatmount images of Rbpms-stained retinas.

<img src="Fig4.png" alt="Design" style="width: 100%; height: auto; ">

<br>

The platform is divided into three parts: axon (CTB) quantification, mean fluorescence intensity (MFI) quantification, and retinal ganglion cell (Rbpms) quantification.

<br>

#### **Axon Quantification**

**Total Number of Axons** - Measure the total number of axons in an image at 4 separate intervals from the injury site. 

<img src="Fig1.png" alt="Design" style="width: 100%; height: auto; ">
*Developed based on 20x images of mouse optic nerves stained with cholera toxin subunit B (CTB).

<br>

**Normalized Number of Axons** - Measure the total number of axons in an image at 4 separate intervals from the injury site, and then normalize the total number of axons by the nerve width (calculated from nuclear stained image pixel height in inches) at each of the 4 intervals, represented by axons / um of nerve width. 

<img src="Fig2.png" alt="Design" style="width: 100%; height: auto; ">}
*Developed based on 20x images of mouse optic nerves stained with cholera toxin subunit B (CTB) & Hoechst.

<br>

**Multiple File Quantification (Total #)** - For multiple image files simultaneously, measure the total number of axons in an image at 4 separate intervals from the injury site. 

<br>

#### **MFI Quantification**  

**Total MFI** - Measure the mean fluorescence intensity (MFI) of an image at 4 separate intervals from the injury site.

<img src="Fig3.png" alt="Design" style="width: 100%; height: auto; ">
*Developed based on 20x images of mouse sciatic nerves stained with Stathmin-2.

<br>

**Normalized MFI** - Measure the MFI in an image at 4 separate intervals from the injury site, and then normalize the MFI by the nerve width (calculated from nuclear stained image and pixel height in inches) at each of the 4 intervals, represented by MFI / um of nerve width. 

<br>

#### **RGC Quantification**

**Single File Quantification** - Measure the number of Rbpms+ cells in a field-of-view of one image file.

**Multiple File Quantification** - For multiple image files simultaneously, measure the number of Rbpms+ cells in a field-of-view.

<br>

<img src="Fig5.png" alt="Design" style="width: 100%; height: auto; ">

*Developed based on 20x images of mouse retinas stained with Rbpms.

<br>

#### **Developer** 

<br>

<img src="Headshot.png" alt="Design" style="width: 50%; height: auto; ">

Matthew Finneran, Giger Laboratory

Neuroscience Graduate Program, University of Michigan Medical School 

Department of Cellular and Developmental Biology

University of Michigan Ann Arbor, MI

2025

<br>

#### **Acknowledgements** 

*Figures created with Biorender.com

*Thank you to Craig Johnson, Bioinformatician of the Cellular and Developmental Biology Department at the University of Michigan, for his help with AxoMetric.

*Thank you to Dr. Larry Benowitz for his feedback and support in the development of this tool.

*Thank you to our support from these funding sources: National Institutes of Health (NIH) Ruth L. Kirschstein National Research Service Award (1F31EY036280-01), the Dr. Miriam and Sheldon G. Adelson Medical Research Foundation, the Stein Innovation Award for Research to Prevent Blindness, and the Department of Health and Human Services Advanced Research Projects Agency for Health (ARPA-H).
<br>


