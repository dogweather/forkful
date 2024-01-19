---
title:                "부분 문자열 추출"
html_title:           "Arduino: 부분 문자열 추출"
simple_title:         "부분 문자열 추출"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/java/extracting-substrings.md"
---

{{< edit_this_page >}}

## 무엇이며 왜 사용하나요?
서브스트링 추출은 문자열의 일부분을 분리하는 프로그래밍 기술입니다. 이를 사용하여 전체 문자열에서 필요한 정보만을 추출하거나 분리하는 경우에 유용합니다.

## 어떻게 사용하나요?

```Java
public class SubstringExample {
 public static void main(String[] args) {
   String str = "해당하는 프로그래밍 언어 찾기";
   String substring = str.substring(4);
   System.out.println(substring);
   // Output: "하는 프로그래밍 언어 찾기"

   String rangeSubstring = str.substring(4, 10);
   System.out.println(rangeSubstring);
   // Output: "하는 프로그"
  }
}
```

## 깊게 알아보기

(1) **역사적 배경**  
서브스트링 추출은 문자열 처리의 기본적인 부분으로 오래 전부터 다양한 프로그래밍 언어에 내장되어 있었습니다.

(2) **대체 방법**  
문자열을 쪼개는 다른 방식으로는 `split()` 함수가 있습니다. 이는 지정한 구분자를 기반으로 문자열을 분리합니다.

(3) **구현 세부정보**  
`substring()` 함수는 원본 문자열을 변경하지 않습니다. 대신 새로운 문자열을 생성하여 반환합니다. start 인덱스는 포함되고, end 인덱스는 포함되지 않는다는 점을 명심하세요.

## 참고자료
- [공식 자바 문서의 substring()](https://docs.oracle.com/javase/7/docs/api/java/lang/String.html#substring(int))
- [split() 함수에 대한 정보](https://www.geeksforgeeks.org/split-string-java-examples/)
- [다양한 문자열 처리 방법](https://www.programiz.com/java-programming/library/string)