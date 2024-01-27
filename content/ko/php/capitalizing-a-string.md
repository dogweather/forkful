---
title:                "문자열 대문자로 변환하기"
date:                  2024-01-19
html_title:           "Arduino: 문자열 대문자로 변환하기"
simple_title:         "문자열 대문자로 변환하기"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/php/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (무엇을 왜?)
문자열 대문자화는 문자열 내의 모든 문자를 대문자로 바꾸는 것입니다. 명확성을 높이거나, UI 일관성을 유지하거나, 프로그램에서 문자열을 표준화하기 위해 사용합니다.

## How to: (하는 방법)
PHP에서 문자열을 대문자로 변환하는 기본 함수는 `strtoupper`입니다. 여기 사용 예제가 있습니다:

```PHP
<?php
$originalString = "hello world";
$capitalizedString = strtoupper($originalString);

echo $capitalizedString; // HELLO WORLD
?>
```

`mb_strtoupper`는 멀티바이트 문자(예: 한글)에 대해서 사용됩니다:

```PHP
<?php
$originalString = "안녕하세요";
$capitalizedString = mb_strtoupper($originalString);

echo $capitalizedString; // 안녕하세요
?>
```

**참고:** PHP의 `mb_strtoupper` 함수는 기본적으로 한글에 대해 대문자로 변환하는 기능을 지원하지 않습니다.

## Deep Dive (심층 분석)
대소문자 변환은 초기 프로그래밍에서부터 있었습니다. 예전에는 데이터를 대문자로 표기하는 것이 일반적이었습니다. 오늘날에는 사용자 인터페이스 및 데이터 처리에서 컨벤션을 일관되게 유지하기 위해 문자열을 대문자로 변환합니다.

대안으로 `ucfirst`와 `ucwords` 함수가 있는데, 각각 문자열의 첫 문자와 모든 단어의 첫 문자를 대문자로 바꿉니다:

```PHP
<?php
$lowerString = "hello world";
echo ucfirst($lowerString); // Hello world
echo ucwords($lowerString); // Hello World
?>
```

구현 세부 사항은 `mb_strtoupper` 함수가 다국어 지원을 위해 구성된 `mbstring` 확장에 의존하는 점에서 복잡해집니다. 멀티바이트 문자열을 처리할 때 주의가 필요합니다.

## See Also (참고할 사항)
- PHP 공식 문서에서 `strtoupper`: https://www.php.net/manual/en/function.strtoupper.php
- PHP 공식 문서에서 `mb_strtoupper`: https://www.php.net/manual/en/function.mb-strtoupper.php
- PHP 문자 인코딩에 대한 정보: https://www.php.net/manual/en/mbstring.supported-encodings.php
