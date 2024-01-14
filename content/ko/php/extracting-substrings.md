---
title:    "PHP: 부분 문자열 추출"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/php/extracting-substrings.md"
---

{{< edit_this_page >}}

## 왜
문자열에서 하위 문자열을 추출하는 것이 왜 중요한지 궁금하신가요? PHP 프로그래밍에서 하위 문자열 추출은 문자열 처리, 텍스트 검색 및 패턴 일치와 같은 다양한 작업에 유용합니다.

## 방법
하위 문자열을 추출하기 위해서는 ```PHP substr()``` 함수를 사용하면 됩니다. 이 함수는 3개의 매개변수를 가지며, 첫 번째 매개변수는 추출할 문자열, 두 번째 매개변수는 하위 문자열의 시작 위치, 세 번째 매개변수는 필요한 문자 수입니다. 예를 들어, ```PHP substr("Hello World", 6, 5)```는 "World"라는 문자열을 추출합니다.

추출된 하위 문자열은 변수에 저장할 수 있으며, 이를 활용하여 더 복잡한 작업을 수행할 수도 있습니다.

```
PHP $str = "Today is a beautiful day";
PHP $substr = substr($str, 11, 9);
PHP echo $substr; // 결과는 "beautiful"입니다.
```

## 깊이 파고들기
PHP에서 하위 문자열 추출은 다양한 유용한 기능을 제공합니다. 하지만 이를 시행하다 보면 알 수 없는 오류나 예상치 못한 결과가 나오는 경우가 있을 수 있습니다. 이를 방지하기 위해 다음 내용을 알아두시면 좋습니다.

- 시작 위치가 음수인 경우, 문자열이 뒤에서부터 추출됩니다. 예를 들어, ```PHP substr("Hello World", -5)```는 "World"를 추출합니다.
- 시작 위치와 필요한 문자 수를 지정하지 않으면, 문자열 전체가 추출됩니다. 즉, ```PHP substr("Hello World")```는 "Hello World"를 반환합니다.
- 필요한 문자 수가 길이보다 클 경우, 나머지 문자는 무시됩니다. 예를 들어, ```PHP substr("Hello", 0, 10)```는 "Hello"를 반환합니다.

더 많은 내용은 PHP 공식 문서를 참고하시면 됩니다.

## 관련 정보
- [PHP substr() 공식 문서](https://www.php.net/manual/ko/function.substr.php)
- [쉽게 따라하는 PHP 문자열 처리 가이드](https://tutorials.microbit.co.kr/php/processing-strings/)
- [PHP와 정규표현식 사용하기](https://webclub.tistory.com/453)