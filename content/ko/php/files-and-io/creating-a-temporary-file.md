---
date: 2024-01-20 17:41:13.862445-07:00
description: "How to: (\uC5B4\uB5BB\uAC8C \uD558\uB098\uC694?) PHP\uC5D0\uC11C \uC784\
  \uC2DC \uD30C\uC77C\uC744 \uB9CC\uB4E4\uACE0 \uC0AC\uC6A9\uD558\uB294 \uBC29\uBC95\
  \uC740 `tmpfile()`\uC640 `tempnam()` \uD568\uC218\uB97C \uC774\uC6A9\uD558\uB294\
  \ \uAC83\uC785\uB2C8\uB2E4. \uBA3C\uC800 `tmpfile()` \uC608\uC81C\uB97C \uBCF4\uC2DC\
  \uC8E0."
isCJKLanguage: true
lastmod: '2024-04-05T21:53:57.079893-06:00'
model: gpt-4-1106-preview
summary: "(\uC5B4\uB5BB\uAC8C \uD558\uB098\uC694?) PHP\uC5D0\uC11C \uC784\uC2DC \uD30C\
  \uC77C\uC744 \uB9CC\uB4E4\uACE0 \uC0AC\uC6A9\uD558\uB294 \uBC29\uBC95\uC740 `tmpfile()`\uC640\
  \ `tempnam()` \uD568\uC218\uB97C \uC774\uC6A9\uD558\uB294 \uAC83\uC785\uB2C8\uB2E4\
  ."
title: "\uC784\uC2DC \uD30C\uC77C \uC0DD\uC131\uD558\uAE30"
weight: 21
---

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
