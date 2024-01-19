---
title:                "텍스트 검색 및 교체"
html_title:           "Elixir: 텍스트 검색 및 교체"
simple_title:         "텍스트 검색 및 교체"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/php/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

##서론 : 무엇이며 왜 사용하는가?
텍스트 검색 및 교체는 특정 문자열을 찾아 다른 문자열로 대체하는 프로세스입니다. 이는 데이터 가공, 지역화 및 리팩토링과 같은 상황에서 광범위하게 사용됩니다.

##사용 방법 :
PHP에서 문자열을 찾아 교체하기 위해 `str_replace()` 함수를 사용합니다. 예제와 출력을 살펴보겠습니다.

```PHP
<?php
$originalText = "Hello World!";
$replacedText = str_replace("World", "PHP", $originalText);
echo $replacedText;
?>
```

이 코드를 실행하면, 결과는 다음과 같이 나옵니다:

```shell
Hello PHP!
```

"World" 문자열이 "PHP" 로 바뀌었습니다.

##딥 다이브 :
**1. 역사적 배경**: PHP에서 텍스트 검색 및 교체는 원래 Perl 언어에서 도입된 개념입니다. PHP의 `str_replace()` 함수는 이 함수를 활성화합니다.

**2. 대체 방법**: `str_replace()` 외에도 `preg_replace()` 함수를 사용하여 정규 표현식을 활용한 더 복잡한 교체 작업을 수행할 수 있습니다.

**3. 구현 세부 정보**: `str_replace()`는 대소 문자를 구분합니다. 대소 문자를 구분하지 않게 하려면 `str_ireplace()` 함수를 사용하십시오.

##추가 정보 :
- PHP 공식 문서:  [str_replace()](https://www.php.net/manual/en/function.str-replace.php) 
- 관련 튜토리얼: [W3Schools PHP str_replace() Function Tutorial](https://www.w3schools.com/php/func_string_str_replace.asp)