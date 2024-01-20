---
title:                "आपलोग कोडिंग की शिक्षा देते हुए और टेस्ट केस बनाने के लिए जीवन और श्रम को निर्दोष रूप से संभालने की प्रक्रिया सीखें"
html_title:           "Swift: आपलोग कोडिंग की शिक्षा देते हुए और टेस्ट केस बनाने के लिए जीवन और श्रम को निर्दोष रूप से संभालने की प्रक्रिया सीखें"
simple_title:         "आपलोग कोडिंग की शिक्षा देते हुए और टेस्ट केस बनाने के लिए जीवन और श्रम को निर्दोष रूप से संभालने की प्रक्रिया सीखें"
programming_language: "Swift"
category:             "Swift"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/swift/writing-tests.md"
---

{{< edit_this_page >}}

## Writing Tests: Kya Aur Kyun?

Aajkal, har koi programmer testing ke baare mein baat karta hai. Lekin kya aapko pata hai ki testing kya hai aur iska kya maqsad hai? Hum apne code mein errors aur bugs ko pakadne ke liye tests likhte hain. Isse humare application ke functionality aur performance ko improve karne mein madad milti hai.

## Kaise Kare?

Hum Swift mein tests likhne ke liye XCTest framework ka istemal karte hain. Ismein hum apne code ko simulate karte hain aur expected output ko compare karte hain. Yeh humare code ke different parts ko test karne mein madad karta hai.

**Example:**

```Swift
// Function to add two numbers
func addNumbers(num1: Int, num2: Int) -> Int {
    return num1 + num2
}

// Test case for addNumbers function
func testAddNumbers() {
    let expectedResult = 5
    let result = addNumbers(num1: 2, num2: 3)

    // Assert statement to compare expected and actual result
    assert(result == expectedResult, "Addition function is not working as expected")
}

// Calling the test function
testAddNumbers()
```

**Output:**
No output means the test passed successfully.

## Gehri Jhaank (Deep Dive)

Testing ke concept mein sabse pehle parikalan ka naam aata hai. Puri tareh se validation ke liye, hume apne code ke har hisse ko test karna zaroori hai. Agar hum code ko manually test karenge, toh yeh ek bahut lamba aur mamooli kaam ban jayega. Isi liye hum automation testing ka istemal karte hain, jisse hume time aur effort ki bachat hoti hai. Kuch log third-party frameworks jaise ki Quick and Nimble ka istemal bhi karte hain test likhne ke liye.

## Juddhein (See Also)

2. [Quick Framework](https://github.com/Quick/Quick)
3. [Nimble Framework](https://github.com/Quick/Nimble)