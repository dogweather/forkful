---
date: 2024-01-20 17:41:13.862445-07:00
description: "PHP\uC5D0\uC11C \uC784\uC2DC \uD30C\uC77C\uC744 \uB9CC\uB4DC\uB294 \uAC83\
  \uC740 \uB370\uC774\uD130\uB97C \uC77C\uC2DC\uC801\uC73C\uB85C \uC800\uC7A5\uD558\
  \uB294 \uACF3\uC774 \uD544\uC694\uD560 \uB54C \uC0AC\uC6A9\uD569\uB2C8\uB2E4. \uC774\
  \uAC83\uC740 \uBCF4\uC548, \uB370\uC774\uD130 \uBD84\uC11D, \uB610\uB294 \uB300\uC6A9\
  \uB7C9 \uCC98\uB9AC\uC5D0\uC11C \uD30C\uC77C\uC758 \uC2E4\uC81C \uACBD\uB85C\uB97C\
  \ \uC228\uAE38 \uB54C \uC720\uC6A9\uD569\uB2C8\uB2E4."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:55.385234-06:00'
model: gpt-4-1106-preview
summary: "PHP\uC5D0\uC11C \uC784\uC2DC \uD30C\uC77C\uC744 \uB9CC\uB4DC\uB294 \uAC83\
  \uC740 \uB370\uC774\uD130\uB97C \uC77C\uC2DC\uC801\uC73C\uB85C \uC800\uC7A5\uD558\
  \uB294 \uACF3\uC774 \uD544\uC694\uD560 \uB54C \uC0AC\uC6A9\uD569\uB2C8\uB2E4. \uC774\
  \uAC83\uC740 \uBCF4\uC548, \uB370\uC774\uD130 \uBD84\uC11D, \uB610\uB294 \uB300\uC6A9\
  \uB7C9 \uCC98\uB9AC\uC5D0\uC11C \uD30C\uC77C\uC758 \uC2E4\uC81C \uACBD\uB85C\uB97C\
  \ \uC228\uAE38 \uB54C \uC720\uC6A9\uD569\uB2C8\uB2E4."
title: "\uC784\uC2DC \uD30C\uC77C \uC0DD\uC131\uD558\uAE30"
weight: 21
---

## What & Why? (무엇이며 왜?)
PHP에서 임시 파일을 만드는 것은 데이터를 일시적으로 저장하는 곳이 필요할 때 사용합니다. 이것은 보안, 데이터 분석, 또는 대용량 처리에서 파일의 실제 경로를 숨길 때 유용합니다.

## How to: (어떻게 하나요?)
PHP에서 임시 파일을 만들고 사용하는 방법은 `tmpfile()`와 `tempnam()` 함수를 이용하는 것입니다. 먼저 `tmpfile()` 예제를 보시죠:

```php
$tempFile = tmpfile();
fwrite($tempFile, "임시 파일에 데이터를 작성합니다.\n");
rewind($tempFile);
echo fread($tempFile, 1024);

fclose($tempFile); // 파일을 닫고, 시스템에서 삭제합니다.
```

`tempnam()` 함수를 사용하는 방법도 있습니다.

```php
$tempDir = sys_get_temp_dir(); // 임시 디렉토리 경로를 가져옵니다.
$tempFile = tempnam($tempDir, 'TMP_');
file_put_contents($tempFile, "임시 파일에 뭔가 씁니다.\n");
echo file_get_contents($tempFile);

unlink($tempFile); // 필요 없어지면 파일을 삭제합니다.
```

## Deep Dive (심층 분석)
임시 파일을 만드는 것은 휘발성 저장공간을 의미합니다. 파일은 열려 있을 때만 존재하고 닫히면 삭제됩니다(`tmpfile()`의 경우). 과거에는 파일 시스템 접근 없이 메모리에서 직접 이런 작업을 처리했습니다만, 대용량 데이터 처리에는 더 큰 저장공간이 필요합니다.

`tempnam()` 함수는 임시 파일을 만들지만, 자동으로는 삭제되지 않기에 `unlink()`로 수동 삭제를 해야 합니다. `tmpfile()` 대신 이를 사용하는 이유는 파일에 이름을 지정할 수 있기 때문입니다.

보안 측면에서, 직접 경로를 숨기면 외부에서 데이터에 접근하기 어렵게 만들 수 있습니다. 이런 방법이 많이 사용되긴 하지만, 보안 인증과 데이터 암호화와 같은 추가 조치를 취하는 것이 좋습니다.

## See Also (참고 자료)
- PHP 공식 문서의 temp 관련 함수들: [PHP: tempnam - Manual](https://www.php.net/manual/en/function.tempnam.php)
- 임시 파일과 보안 관련 권장사항: [OWASP Guide to Secure Data Handling](https://owasp.org/www-project-top-ten/)
