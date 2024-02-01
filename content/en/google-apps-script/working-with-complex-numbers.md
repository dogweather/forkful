---
title:                "Working with complex numbers"
date:                  2024-02-01T13:42:14.587962-07:00
model:                 gpt-4-0125-preview
simple_title:         "Working with complex numbers"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/google-apps-script/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## What & Why?

Dealing with complex numbers isn't everyday stuff in Google Apps Script, but when you hit a scenario that needs them (think engineering calculations, advanced math stuff), they're indispensable. You're basically diving into a mathematical concept where numbers have a real part and an imaginary part, usually represented as a + bi, where "i" is the square root of -1. 

## How to:

Google Apps Script doesn't have built-in support for complex numbers, but that doesn't mean we can't play around with them. Let's create our own little framework to handle complex numbers.

First, we need a way to represent a complex number. We'll use an object for this:

```Google Apps Script
function Complex(real, imaginary) {
  this.real = real;
  this.imaginary = imaginary;
}

// Adding two complex numbers
function addComplex(c1, c2) {
  return new Complex(c1.real + c2.real, c1.imaginary + c2.imaginary);
}

// Multiplying two complex numbers
function multiplyComplex(c1, c2) {
  return new Complex(c1.real * c2.real - c1.imaginary * c2.imaginary,
                     c1.real * c2.imaginary + c1.imaginary * c2.real);
}

// Example usage
var num1 = new Complex(3, 4);
var num2 = new Complex(1, 2);

var sum = addComplex(num1, num2);
var product = multiplyComplex(num1, num2);

Logger.log("Sum: " + sum.real + " + " + sum.imaginary + "i"); // Sum: 4 + 6i
Logger.log("Product: " + product.real + " + " + product.imaginary + "i"); // Product: -5 + 10i
```

This example lays out a basic structure for working with complex numbers, including adding and multiplying them. For more complex operations (pun intended), you'd expand these concepts further.

## Deep Dive

Historically, complex numbers were a bit controversial, mainly because they involve the square root of negative one, a concept that didn't sit well with the math folks for centuries. In the realm of computer programming, most languages, including those scripting engines for the web, didn't originally bake in support for complex numbers at the ground level, Google Apps Script included. This means that when we need to work with them, we're implementing these mathematical concepts on our own, or leveraging libraries that others have written.

For serious mathematical and scientific computing in Google Apps Script that requires complex numbers, sometimes it's worth considering other platforms that natively support complex numbers, like Python with its extensive libraries (e.g., NumPy). However, for light lifting or when Apps Script is a requirement or fits well with your project (think Google Sheets custom functions), the approach we discussed works reasonably well and is pretty fun to boot. It's a fine example of how understanding underlying computer science concepts allows us to extend the capabilities of our chosen tools, even if they don't support our needs right out of the box.
