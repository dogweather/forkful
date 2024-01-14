---
title:    "TypeScript: टेस्ट लिखना"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

# Kyu (Why)
Unit tests likhne ke shamil hone ka karan kya hai? Unit tests likhna zaroori kyu hai? Ye sawal shayad aapke dimaag me uthta hoga jab aap coding karte hai. Unit tests likhna ek bahut hi ahem topic hai jo developers ke liye bahut mahatvapurna hai. Is blog post me hum dekhenge ki unit tests likhne ke kya fayde hai aur kaise hum inhe likh sakte hai.

# Kaise Kare (How To)
### TypeScript me Test Case Kaise Banye?
Unit tests likhne ke dauran hume TypeScript ka istemal karna chaiye kyu ki vo bahut hi saral aur asaan hai. Chaliye shuru karte hai:

Sabse pehle, hum ek class banyege jiska naam "Math" hoga. Ye class simple mathematical operations jaise addition, subtraction, multiplication aur division ko handle karega. Is class me ek default constructor hona chaiye jo teen variables accept karta hai - num1, num2 aur operator. Humara code niche diya gaya hai:

```TypeScript
class Math {
  constructor(num1: number, num2: number, operator: string) {
    this.num1 = num1;
    this.num2 = num2;
    this.operator = operator;
  }

  num1: number;
  num2: number;
  operator: string;

  add(): number {
    return this.num1 + this.num2;
  }

  subtract(): number {
    return this.num1 - this.num2;
  }

  multiply(): number {
    return this.num1 * this.num2;
  }

  divide(): number {
    return this.num1 / this.num2;
  }
}

```
Ab hume apne class ko test karna hai. Iske liye hum chaiye chaiye chaiye ye kuch steps follow kare:

1. Pehle chaiye chaiye chaiye chaiye chaiye chaiye chaiye chaiye ki hum ek "math.test.ts" file banye.
2. Is file ke andar chaiye chaiye chaiye chaiye chaiye chaiye chaiye chaiye kuch chaiye chaiye chaiye chaiye chaiye chaiye chaiye chaiye chaiye chaiye chaiye chaiye chaiye chaiye chaiye chaiye chaiye chaiye chaiye chaiye chaiye chaiye chaiye ```TypeScript chaiye chaiye chaiye chaiye chaiye chaiye chaiye chaiye chaiye chaiye chaiye chaiye chaiye chaiye chaiye chaiye chaiye [[ chaiye chaiye chaiye chaiye ]] chaiye chaiye chaiye chaiye chaiye chaiye chaiye chaiye chaiye chaiye chaiye chaiye chaiye chaiye chaiye chaiye chaiye chaiye chaiye chaiye chaiye chaiye chaiye chaiye chaiye new Math(5, 2, '+');``` chaiye chaiye chaiye chaiye chaiye chaiye chaiye chaiye chaiye chaiye chaiye chaiye chaiye chaiye chaiye chaiye chaiye chaiye chaiye chaiye chaiye chaiye chaiye chai