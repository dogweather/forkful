---
title:    "Java: 문자열을 소문자로 변환하기"
keywords: ["Java"]
---

{{< edit_this_page >}}

## 왜
이번 프로그래밍 기술 블로그는 문자열을 소문자로 변환하는 과정에 대해 소개해드립니다. 이 기술을 배우는 이유는 다양한 데이터 분석 혹은 검색 기능을 개발할 때 문자열을 일관된 형식으로 처리할 수 있어 효율적인 프로그래밍이 가능하기 때문입니다.

## 어떻게
문자열을 소문자로 변환하는 방법은 간단합니다. 우선, 문자열을 새로운 변수에 저장한 뒤, `toLowerCase()` 메서드를 이용하면 됩니다. 아래는 예시 코드와 실행 결과입니다.
```Java
// 문자열을 변수에 저장
String name = "SHERLOCK HOLMES";

// 문자열을 소문자로 변환
String lowerCaseName = name.toLowerCase();

// 변환된 결과 출력
System.out.print(lowerCaseName);
```

실행 결과: sherlock holmes

## 딥 다이브
위에서 설명한 방법은 간단하지만, 실제로는 어떻게 동작하는지 알고 싶으신 분들을 위해 좀 더 깊게 들어가보겠습니다. `toLowerCase()` 메서드는 문자열의 각 문자를 비교해 대문자일 경우 소문자로 변경하고, 이미 소문자인 경우 변경하지 않는 방식으로 동작합니다. 이를 기반으로 한 알고리즘을 구현한 `toLowerCase()` 메서드의 예시 코드는 다음과 같습니다.
```Java
// "a"의 아스키 코드 값: 97, "A"의 아스키 코드 값: 65
// 대문자와 소문자의 아스키 코드 값 차이는 32
// 따라서 대문자를 소문자로 변환하기 위해 32를 더한 값으로 변경

// String 객체에서 사용할 문자 배열 생성
char[] charArr = name.toCharArray();

// 각 문자를 비교해 대문자인 경우 +32를 한 후 다시 대입
for(int i = 0; i < charArr.length; i++){
  if(charArr[i] >= 65 && charArr[i] <= 90){
    charArr[i] = (char) (charArr[i] + 32);
  }
}

// 결과적으로 변경된 문자 배열을 다시 String 객체로 변환
String lowerCaseName = new String(charArr);
```
이와 같은 방식으로 `toLowerCase()` 메서드가 동작하여 문자열을 소문자로 변경합니다.

## 참고자료
- [String 클래스 JavaDoc](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [ASCII 테이블](https://ko.wikipedia.org/wiki/ASCII)
- [Java에서 문자열 다루기](https://www.tutorialspoint.com/java/java_string_tolowercase.htm)

## 참고
본 포스트에서 공유한 예시 코드는 실제 프로그램에서 사용하기 위해 축약 처리한 것이므로 예시 코드 그대로 사용하지 않는 것을 권장합니다.