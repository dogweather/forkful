---
title:                "디렉토리 존재 여부 확인하기"
date:                  2024-01-20T14:57:40.822173-07:00
html_title:           "Fish Shell: 디렉토리 존재 여부 확인하기"
simple_title:         "디렉토리 존재 여부 확인하기"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/php/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
디렉토리 존재 여부 확인은 파일 시스템에서 특정 폴더가 실제로 있나 찾아보는 것입니다. 프로그래머들은 데이터 저장, 읽기, 바꾸기 전에 에러를 피하기 위해 이 작업을 합니다.

## How to: (어떻게 하나요?)
```PHP
<?php
$directory = "/path/to/directory";

// Check if the directory exists
if (is_dir($directory)) {
    echo "The directory exists.";
} else {
    echo "The directory does not exist.";
}
?>
```

이 코드는 `$directory`에 지정된 경로가 존재하는지 확인합니다. 폴더가 있으면 "The directory exists."를, 없으면 "The directory does not exist."를 출력합니다.

## Deep Dive (심층 분석)
디렉토리 존재 여부 확인 기능은 오래전부터 있었습니다. PHP에서 `is_dir`함수는 PHP 4 버전 때부터 사용할 수 있었어요. `file_exists` 함수로도 폴더 또는 파일 존재 여부를 체크할 수 있지만, 주로 파일 존재 여부에 쓰입니다. 성능 측면에서는 `is_dir` 함수가 특정 경로가 디렉토리인지만 확인하면 되니까 `file_exists` 함수보다 조금 더 빨라요. 이런 차이 때문에 디렉토리인지 아닌지 분명히 구분해야 할 때는 `is_dir` 함수를 사용하는 것이 좋습니다.

## See Also (함께 보기)
- PHP 공식 문서에서 `is_dir` 함수에 대한 정보: [php.net/manual/en/function.is-dir.php](https://www.php.net/manual/en/function.is-dir.php)
- PHP `file_exists` 함수에 대한 자세한 설명: [php.net/manual/en/function.file-exists.php](https://www.php.net/manual/en/function.file-exists.php)
- 파일 시스템 관련 PHP 함수 목록: [php.net/manual/en/ref.filesystem.php](https://www.php.net/manual/en/ref.filesystem.php)
