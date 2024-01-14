---
title:    "Java: 문자열 연결하기"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/java/concatenating-strings.md"
---

{{< edit_this_page >}}

## 왜

문자열 연결을 하는 이유는 매우 단순합니다. 
다양한 문자열을 결합해서 하나의 새로운 문자열로 만들 수 있기 때문입니다. 
이는 문자열을 다루는 데 있어 매우 유용한 기능입니다.

## 어떻게

```Java
String playerName = "John";
String game = "Minecraft";
String message = playerName + " has just entered the world of " + game;
System.out.println(message);
```
**출력 결과:**
John has just entered the world of Minecraft

문자열을 연결하기 위해서는 "+" 연산자를 사용하면 됩니다. 
위의 예시 코드처럼, 여러 개의 문자열을 "+"로 연결할 수 있습니다. 
연결된 문자열은 새로운 변수에 저장할 수도 있고, 바로 출력할 수도 있습니다.

## 딥 다이브

문자열을 연결하는 데는 여러 가지 방법이 있습니다. 가장 기본적인 방법은 위의 예시 코드처럼 "+" 연산자를 사용하는 방법입니다. 
또 다른 방법으로는 **StringBuffer** 또는 **StringBuilder** 클래스를 사용하는 방법도 있습니다. 
이들 클래스는 문자열을 바로 연결하는 것이 아니라, 연결될 문자열을 미리 저장해놓고 한 번에 연결하는 방식을 사용합니다. 
이는 큰 규모의 문자열을 연결할 때 성능의 향상을 가져올 수 있습니다.

## 알아두면 좋은 사항

문자열 연결 시에는 조심해야 할 점이 있습니다. 
연결할 문자열이 많아지면 많아질수록 성능에 영향을 주기 때문입니다. 
또한 문자열을 계속 수정하면서 연결하는 일은 원하지 않는 결과를 초래할 수 있습니다. 
따라서 문자열 연결을 많이 사용할 경우에는 위의 언급한 **StringBuffer**나 **StringBuilder**를 사용하는 것이 좋습니다.

## See Also

- [String Concatenation in Java](https://www.geeksforgeeks.org/string-concatenation-in-java/)
- [StringBuffer vs StringBuilder in Java](https://www.geeksforgeeks.org/stringbuffer-vs-stringbuilder-in-java/)
- [Java String 연결 방법 비교](https://www.codeflow.site/ko/article/java-string-concatenation)