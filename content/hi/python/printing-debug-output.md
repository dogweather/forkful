---
title:                "डिबग आउटपुट प्रिंट करना"
html_title:           "Python: डिबग आउटपुट प्रिंट करना"
simple_title:         "डिबग आउटपुट प्रिंट करना"
programming_language: "Python"
category:             "Python"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/python/printing-debug-output.md"
---

{{< edit_this_page >}}

## Kya aur Kyun?
Printing debug output ka matlab hai ki hum apne code ki running process ko track karna aur samajhna. Programmers is liye ise karte hai kyunki isse unko unke code mein errors aur bugs ko dhoondhne mein madad milti hai. Yeh ek important tool hai programming mein jo errors ko identify aur fix karne mein kaam aata hai.

## Kaise kiya jaye?
Python mein debug output print karne ke liye hum print() function ka upyog karte hai. Niche diye gaye code blocks mein humne kuch examples diye hai jinse aap samajh sakte hai ki kaise hum debug output ko print karte hai.

```
# Example 1
age = 25
print("My age is", age)

# Example 2
first_name = "John"
last_name = "Doe"
print("My name is", first_name, last_name)

# Example 3
numbers = [1, 2, 3, 4, 5]
sum = 0
for num in numbers:
  sum += num
  print("Current sum is:", sum)
```

Output:
```
My age is 25
My name is John Doe
Current sum is: 1
Current sum is: 3
Current sum is: 6
Current sum is: 10
Current sum is: 15
```

## Gehri Khurak:
Debug output printing ke liye humne print() function ka upyog kiya, lekin aur bhi tarike hai jaise debug mode, log files, aur debugging tools. In sabke alawa, hum apne code mein comments ya debugging statements bhi add kar sakte hai jo hume errors aur bugs ko pakadne mein madad karte hai. Debug output se related kuch aur articles neeche links mein diye gaye hai jo aapke liye helpful ho sakte hai.

## Related Links:
- [Python Debugging Tutorial](https://realpython.com/python-debugging-pdb/)
- [Best Practices for Debugging in Python](https://medium.com/datadriveninvestor/best-practices-for-debugging-in-python-2-7-b2e7c75e6570)
- [Debugging in Python: The Absolute Basics](https://www.debugging-tutorial.com/debugging-python/the-absolute-basics/)