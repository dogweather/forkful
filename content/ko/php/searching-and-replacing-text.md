---
title:                "텍스트 검색 및 대체"
html_title:           "PHP: 텍스트 검색 및 대체"
simple_title:         "텍스트 검색 및 대체"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/php/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
텍스트 검색 및 교체는 프로그래머가 자주 하는 작업 중 하나입니다. 이는 코드에서 일부 텍스트를 찾고 다른 텍스트로 대체하는 것을 의미합니다. 이것은 코드를 좀 더 간결하고 유지 보수하기 쉽게 만듭니다.

## 방법:
```PHP
// strlen() 함수 사용하기
$str = "Hello world!";
echo "Hello가 있는지 검색: " . strpos($str, "Hello"); //출력: 0 (세 번째 인덱스)
echo "Goodbye가 있는지 검색: " . strpos($str, "Goodbye"); //출력: 아무것도 출력되지 않음
// 문자열 교체하기
$oldStr = "Hello world!";
$newStr = str_replace("world", "Korea", $oldStr);
echo $newStr; //출력: Hello Korea!
```

## 깊게 파고들기:
텍스트 검색 및 교체는 프로그래밍 언어에서 오랜 역사를 가지고 있으며, 많은 프로그래밍 언어에서 지원합니다. 예를 들어, C 언어에서는 ```strstr()``` 함수를 사용하고, Python에서는 ```re.sub()``` 함수를 사용합니다. 따라서 적절한 함수를 사용하기 위해서는 언어의 문서를 읽는 것이 중요합니다.

## 관련 자료:
- [PHP 문자열 함수와 사용법](https://www.w3schools.com/php/php_ref_string.asp)
- [C 언어 strstr() 함수 문서](https://www.tutorialspoint.com/c_standard_library/c_function_strstr.htm)
- [Python re.sub() 함수 문서](https://docs.python.org/3/library/re.html#re.sub)