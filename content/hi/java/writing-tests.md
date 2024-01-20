---
title:                "टेस्ट लेखन"
html_title:           "Java: टेस्ट लेखन"
simple_title:         "टेस्ट लेखन"
programming_language: "Java"
category:             "Java"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/java/writing-tests.md"
---

{{< edit_this_page >}}

# Kya & Kyu?
Testing ek samasya hai jo har programmer ko aati hai. Lekin ye zaroori hai kyuki testing ke bina humare code par kabhi bharosa nahi ho sakta. Testing ke madhyam se hum apne code ki quality ko check karte hai aur bugs ko pehle se hi pata lagate hai, jisse unhe baad me theek karna asaan hojata hai.

# Kaise kare: 
```Java 
public class Calculator {
  public static void main(String[] args) {
    // Hum apne program me ek simple calculator banayenge
    int num1 = 5;
    int num2 = 10;

    // Addition
    int sum = num1 + num2;
    System.out.println("Addition: " + sum); // Output: Addition: 15

    // Subtraction
    int diff = num2 - num1;
    System.out.println("Subtraction: " + diff); // Output: Subtraction: 5

    // Multiplication
    int product = num1 * num2;
    System.out.println("Multiplication: " + product); // Output: Multiplication: 50

    // Division
    int quotient = num2 / num1;
    System.out.println("Division: " + quotient); // Output: Division: 2

    // Modulus
    int remainder = num2 % num1;
    System.out.println("Modulus: " + remainder); // Output: Modulus: 0
  }
}
```
Is example me humne ek simple calculator banaya hai jisme hum addition, subtraction, multiplication, division aur modulus operations perform kar rahe hai.

# Gehri Jhaanki:
Testing ka concept pehli baar 1950s me introduce hua tha. Exponentially growing software industry ke saath saath, testing techniques bhi badhte gaye. Aaj kal kayi alternatives available hai jaise manual testing, automated testing, unit testing, integration testing, system testing, regression testing aur bahut kuch.

# Aur Bhi Dekhe:
- [JUnit](https://junit.org/junit5/) - A popular Java testing framework