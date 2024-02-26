---
date: 2024-01-26 03:41:15.936457-07:00
description: "PHP\uC5D0\uC11C \uBB38\uC790\uC5F4\uC5D0\uC11C \uB530\uC634\uD45C\uB97C\
  \ \uC81C\uAC70\uD55C\uB2E4\uB294 \uAC83\uC740 \uCF54\uB4DC \uB17C\uB9AC\uB098 \uB370\
  \uC774\uD130\uBCA0\uC774\uC2A4 \uCFFC\uB9AC\uB97C \uBC29\uD574\uD560 \uC218 \uC788\
  \uB294 \uADF8 \uC131\uAC00\uC2E0 \uC774\uC911(`\"`) \uB610\uB294 \uB2E8\uC77C(`'`)\
  \ \uB530\uC634\uD45C \uBB38\uC790\uB97C \uC81C\uAC70\uD558\uB294 \uAC83\uC744 \uC758\
  \uBBF8\uD569\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB294 \uC774\uB97C \uD1B5\
  \uD574 \uC785\uB825 \uB370\uC774\uD130\uB97C \uC815\uB9AC\uD558\uAC70\uB098 \uC0B4\
  \uADE0\uD558\uC5EC \uBB38\uC790\uC5F4\uC774 \uC548\uC804\uD558\uAC8C \uC0AC\uC6A9\
  \uB418\uAC70\uB098 \uC800\uC7A5\uB418\uB3C4\uB85D\u2026"
lastmod: '2024-02-25T18:49:52.333901-07:00'
model: gpt-4-0125-preview
summary: "PHP\uC5D0\uC11C \uBB38\uC790\uC5F4\uC5D0\uC11C \uB530\uC634\uD45C\uB97C\
  \ \uC81C\uAC70\uD55C\uB2E4\uB294 \uAC83\uC740 \uCF54\uB4DC \uB17C\uB9AC\uB098 \uB370\
  \uC774\uD130\uBCA0\uC774\uC2A4 \uCFFC\uB9AC\uB97C \uBC29\uD574\uD560 \uC218 \uC788\
  \uB294 \uADF8 \uC131\uAC00\uC2E0 \uC774\uC911(`\"`) \uB610\uB294 \uB2E8\uC77C(`'`)\
  \ \uB530\uC634\uD45C \uBB38\uC790\uB97C \uC81C\uAC70\uD558\uB294 \uAC83\uC744 \uC758\
  \uBBF8\uD569\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB294 \uC774\uB97C \uD1B5\
  \uD574 \uC785\uB825 \uB370\uC774\uD130\uB97C \uC815\uB9AC\uD558\uAC70\uB098 \uC0B4\
  \uADE0\uD558\uC5EC \uBB38\uC790\uC5F4\uC774 \uC548\uC804\uD558\uAC8C \uC0AC\uC6A9\
  \uB418\uAC70\uB098 \uC800\uC7A5\uB418\uB3C4\uB85D\u2026"
title: "\uBB38\uC790\uC5F4\uC5D0\uC11C \uB530\uC634\uD45C \uC81C\uAC70\uD558\uAE30"
---

{{< edit_this_page >}}

## 무엇 & 왜?
PHP에서 문자열에서 따옴표를 제거한다는 것은 코드 논리나 데이터베이스 쿼리를 방해할 수 있는 그 성가신 이중(`"`) 또는 단일(`'`) 따옴표 문자를 제거하는 것을 의미합니다. 프로그래머는 이를 통해 입력 데이터를 정리하거나 살균하여 문자열이 안전하게 사용되거나 저장되도록 합니다.

## 방법:
PHP의 내장 함수를 사용하는 간단한 예입니다:

```php
$quotedString = "'Hello,' she said, \"It's a fine day!\"";
$unquotedString = str_replace(array("'", "\""), '', $quotedString);
echo $unquotedString; // 출력: Hello, she said, Its a fine day!
```

단순하죠? 이 `str_replace()` 함수는 문자열에서 제거할 문자 배열을 취하며, 단일 따옴표와 이중 따옴표를 모두 포함합니다.

## 심층 분석
PHP의 초기 시절에는, 특히 데이터베이스에 데이터를 삽입할 때 문자열에서 따옴표를 조심스럽게 다뤄야 했습니다. 제대로 처리되지 않은 따옴표는 SQL 주입 공격으로 이어질 수 있었습니다. 여기서 마법 따옴표, 즉 입력 데이터를 자동으로 이스케이프하는 기능이 등장했습니다. 이는 나쁜 코딩 습관과 보안 문제를 유도했기 때문에 폐지되었고 결국 삭제되었습니다.

이제는 `str_replace()` 함수나 `preg_replace()`와 같은 정규 표현식을 사용해 더 복잡한 패턴에 대해 이야기하고 있습니다. 정규 표현식 예제는 다음과 같습니다:

```php
$quotedString = "'Hello,' she said, \"It's a fine day!\"";
$unquotedString = preg_replace('/[\'"]/', '', $quotedString);
echo $unquotedString;
```

JSON 데이터의 경우, 따옴표에서 추가 백슬래시를 피하기 위해 `json_encode()`와 `JSON_UNESCAPED_SLASHES | JSON_UNESCAPED_UNICODE`와 같은 옵션을 사용할 수 있습니다.

구현할 때는 엣지 케이스를 고려하세요. 문자열이 특정 따옴표를 포함해야 한다면 어떨까요? 예를 들어 이야기 속의 대화나 측정치의 인치처럼 말이죠. 컨텍스트는 중요하므로, 데이터의 의도된 사용에 맞게 따옴표 제거를 조정하세요.

## 참고
- [PHP: str_replace](https://www.php.net/manual/en/function.str-replace.php)
- [PHP: preg_replace](https://www.php.net/manual/en/function.preg-replace.php)
- [PHP: json_encode](https://www.php.net/manual/en/function.json-encode.php)
- [OWASP: SQL 주입 방지](https://owasp.org/www-community/attacks/SQL_Injection)
