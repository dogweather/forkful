---
title:                "디렉토리가 존재하는지 확인하기"
html_title:           "PHP: 디렉토리가 존재하는지 확인하기"
simple_title:         "디렉토리가 존재하는지 확인하기"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/php/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 무엇인가요? 왜 확인하는 걸까요?

디렉토리가 존재하는지 확인하는 것은 단순한 과업입니다. 프로그래머들이 이 작업을 하는 이유는 자신이 작성한 코드가 올바르게 동작하고, 마주하는 에러를 최소화하기 위해입니다.

## 방법:

```
<?php
// 디렉토리 경로 정의
$directory = "/Users/username/Desktop/";

// 디렉토리 확인하기
if (is_dir($directory)) {
  echo "이 디렉토리는 존재합니다.";
} else {
  echo "이 디렉토리는 존재하지 않습니다.";
}
?>
```

출력:
```
이 디렉토리는 존재합니다.
```

## 깊게 들어가보기:

(1) 과거의 문맥: 디렉토리 존재 확인은 초창기 컴퓨터 시스템에서 매우 중요한 기능이었습니다. 파일 시스템의 개념이 생기기 전, 모든 파일들은 하나의 디렉토리에 있었고, 디렉토리를 관리하는 것은 컴퓨터 시스템의 기반적인 요소였습니다. (2) 대안: 디렉토리 존재 확인을 위해서는 PHP 함수 이외에도 다른 방법들이 있습니다. 예를 들어, 운영체제 기반의 명령어를 사용할 수도 있습니다. (3) 구현 세부 사항: PHP에서 디렉토리 존재 확인은 is_dir() 함수를 사용하여 쉽게 구현할 수 있습니다. 이 함수는 디렉토리의 경로를 매개 변수로 받고, 존재하면 true를, 그렇지 않으면 false를 반환합니다. 

## 참고 자료:

- [Official PHP Documentation on is_dir()](https://www.php.net/manual/en/function.is-dir.php)
- [Alternative ways to check if directory exists in PHP](https://stackoverflow.com/questions/4914577/checking-if-a-directory-exists-in-php/4914603#4914603)
- [Directory and Path in Computing History](https://www.techopedia.com/definition/3787/directory)