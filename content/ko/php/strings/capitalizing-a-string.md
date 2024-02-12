---
title:                "문자열 대문자화"
date:                  2024-02-03T19:06:07.914379-07:00
model:                 gpt-4-0125-preview
simple_title:         "문자열 대문자화"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/php/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇 & 왜?
문자열의 첫 글자를 대문자로 변경하는 것은 주어진 텍스트의 첫 번째 문자를 대문자로 수정하여 문장, 제목 또는 적절한 이름이 데이터셋에서 올바르게 시작하도록 하는 과정을 포함합니다. 프로그래머들은 종종 데이터 정규화, 가독성 향상 또는 사용자 입력이나 텍스트 데이터 처리의 일관성을 보장하기 위해 문자열 대문자화를 실행합니다.

## 어떻게?
PHP는 다양한 목적을 위해 서비스하는 여러 문자열 대문자화 함수를 네이티브로 지원합니다. 다음은 그것들을 사용하는 방법입니다:

### 문자열의 첫 글자를 대문자로 변경:

```php
$string = "hello, world!";
$capitalizedString = ucfirst($string);
echo $capitalizedString; // 출력: Hello, world!
```

### 각 단어의 첫 글자를 대문자로 변경:

```php
$string = "hello, world!";
$capitalizedWords = ucwords($string);
echo $capitalizedWords; // 출력: Hello, World!
```

### 전체 문자열을 대문자로 변환:

```php
$string = "hello, world!";
$upperCaseString = strtoupper($string);
echo $upperCaseString; // 출력: HELLO, WORLD!
```

더 맞춤화된 시나리오나 서드파티 솔루션을 요구할 때 `mbstring`(멀티바이트 문자열용)과 같은 라이브러리를 특히 기본 ASCII 세트를 넘어서는 문자를 다루는 국제화 작업에서 활용할 수 있습니다.

### mbstring을 사용하여 UTF-8 문자열의 첫 글자를 대문자로 변경:

PHP 구성에서 `mbstring` 확장 기능이 활성화되어 있는지 확인한 다음:

```php
$string = "élégant";
$capitalizedString = mb_convert_case($string, MB_CASE_TITLE, "UTF-8");
echo $capitalizedString; // 출력: Élégant
```

이 접근 방식은 ASCII가 아닌 문자를 포함하는 문자열을 정확하게 대문자화하는 데 도움이 되며, 다양한 언어의 미묘한 차이를 준수합니다.
