---
title:    "Java: 텍스트 검색 및 변경"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/java/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## 왜
텍스트를 검색하고 대체하는 것에 참여하는 이유는 프로그래밍에서 매우 중요합니다. 특정 문자열을 다른 문자열로 전환하는 것은 코드를 보다 쉽게 수정하고 유지 관리하는 데 도움이 됩니다.

## 해하는 방법
```Java
// 예시 코드
public static String replaceText(String input, String search, String replace) {
    String output = input.replaceAll(search, replace);
    return output;
}

public static void main(String[] args) {
    String input = "Hello, World!";
    String inputSearch = "Hello";
    String inputReplace = "Hey";
    String output = replaceText(input, inputSearch, inputReplace);
    
    System.out.println(output); // Hey, World!
}
```

이 예시 코드에서는 `replaceAll()` 메소드를 사용하여 특정 문자열을 다른 문자열로 대체하는 방법을 보여줍니다. 이를 통해 코드에서 원하는 문자열을 찾아서 한 번에 대체할 수 있습니다.

## 더 깊이 들어가기
텍스트를 검색하고 대체하는 데 사용할 수 있는 다양한 메소드가 있습니다. `replaceAll()` 외에도 `replaceFirst()`를 사용하여 첫 번째 일치하는 문자열만 대체할 수도 있습니다. 또는 `indexOf()`와 `substring()`을 이용하여 특정 문자열을 찾아 대체하는 방법도 있습니다. 알아두면 프로그래밍에서 더 다양한 상황에 유용하게 사용할 수 있습니다.

## 한 번에 다 대체가 필요한가요?
문자열을 검색하고 대체하는 것보다 정규표현식을 사용하여 한 번에 다 대체하는 것이 효율적일 수 있습니다. 정규표현식은 패턴 매칭을 사용하여 원하는 문자열을 한 번에 찾아서 대체할 수 있습니다. 하지만 정규표현식은 다루기 어렵고 따로 학습이 필요합니다.

## 더 읽어보기
- [Java String replaceAll() method](https://www.javatpoint.com/java-string-replaceall)
- [Java String replaceFirst() method](https://www.w3schools.com/java/ref_string_replacefirst.asp)
- [Java String indexOf() method](https://www.geeksforgeeks.org/java-string-indexof-method-example/)
- [Java String substring() method](https://www.tutorialspoint.com/java/lang/string_substring.htm)
- [정규표현식에 대해 알아보기](https://www.regular-expressions.info/)