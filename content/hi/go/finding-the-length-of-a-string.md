---
title:                "अक्षरों की लंबाई ढूँढना"
html_title:           "Go: अक्षरों की लंबाई ढूँढना"
simple_title:         "अक्षरों की लंबाई ढूँढना"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/go/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
Finding the length of a string क्या है और क्यों programmers इसे करते हैं।

एक string की लंबाई को निकालना strings के लिए एक दिग्गज काम है। programmers इसका उपयोग करके उपयोगकर्ताओं के द्वारा प्रदान किए गए डेटा की प्रबंधन और प्रोग्रामिंग के साथ संबंधित कार्यों को सुनाई पड़ती है।

## कैसे करें?
```Go
func main() {
    str := "Hello, world!" // string declared
    fmt.Println("Length of string:", len(str)) // output: 13 (includes spaces)
}
```

```Go
func main() {
    str := "こんにちは、世界！" // string declared (Japanese translation of "Hello, world!")
    fmt.Println("Length of string:", len(str)) // output: 15 (includes non-English characters)
}
```

## गहराई में खोजें
1. इतिहास के संदर्भ में, strings की लंबाई को प्राप्त करना C language से लेकर शुरू हुआ। 
2. अन्य भाषाओं में भी, strings की लंबाई को निकालने के लिए विभिन्न तरीको का प्रयोग किया जाता है। 
3. Go language में, string की लंबाई को निकालने के लिए len() function का प्रयोग किया जाता है।

## अन्य संबंधित स्रोत
[The Go Programming Language](https://golang.org/)- Official Website 
[Go By Example](https://gobyexample.com/strings)- String manipulation using Go 
[GeeksforGeeks](https://www.geeksforgeeks.org) - Online resource for programming concepts and tutorials.