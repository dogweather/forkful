---
title:                "패턴에 일치하는 문자 삭제"
html_title:           "Fish Shell: 패턴에 일치하는 문자 삭제"
simple_title:         "패턴에 일치하는 문자 삭제"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/php/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

문자열에서 특정 패턴에 해당하는 문자를 삭제하는 것은 프로그램에서 불필요하거나 원치 않는 문자를 제거하여 원하는 결과를 얻기 위해 필요합니다. 코드를 깔끔하게 관리하고, 데이터를 청결하게 유지하는데 도움을 줍니다.

## 동작 방법:

PHP에서, `preg_replace()` 함수를 사용하여 이 작업을 수행할 수 있습니다. 아래 코드는 사용 방법을 보여줍니다.

```PHP
<?php
$string = "Hello, world!";
$pattern = "/[wrl]/";
$replacement = '';
$clean_string = preg_replace($pattern, $replacement, $string);

echo $clean_string;
?>
```

위의 코드는 'w', 'r', 'l'에 해당하는 모든 문자를 찾아서 삭제합니다. 출력은 "Heo, od!"로 표시됩니다.

## 깊게 이해하기

`preg_replace` 함수는 Perl 스타일의 정규 표현식을 사용하여 일치하는 패턴을 찾아 냅니다. 이 기능은 이전 PHP 버전에서도 사용하였으며 현재도 PHP의 핵심 기능 중 하나입니다.

대안으로는 `str_replace` 함수를 사용하여 단순한 문자 혹은 문자열의 대체를 수행할 수 있습니다. 하지만, 이 함수는 정규 표현식을 지원하지 않으므로 `preg_replace`에 비해 덜 유연한 방법입니다.

실제로 PHP가 문자 패턴을 찾아 삭제하는 방식은 문자열을 순회하면서 각 문자를 패턴과 비교하는 것입니다. 일치하는 패턴을 찾으면 해당 문자열 위치에서 문자를 제거하고, 나머지 부분을 합칩니다.

## 참고 자료

1. PHP 공식 문서 `preg_replace()` 함수 페이지: [http://php.net/manual/en/function.preg-replace.php](http://php.net/manual/en/function.preg-replace.php)
2. PHP 공식 문서 `str_replace()` 함수 페이지: [http://php.net/manual/en/function.str-replace.php](http://php.net/manual/en/function.str-replace.php)
3. 자세한 정보와 참고 자료를 위한 StackOverflow 토픽: [https://stackoverflow.com/questions/1252693/using-str-replace-so-that-it-only-acts-on-the-first-match](https://stackoverflow.com/questions/1252693/using-str-replace-so-that-it-only-acts-on-the-first-match)