---
title:                "텍스트 파일 읽기"
html_title:           "PHP: 텍스트 파일 읽기"
simple_title:         "텍스트 파일 읽기"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/php/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 무엇이며 왜 해야 할까?

텍스트 파일을 읽는 것은 프로그래머가 파일에 저장된 정보를 읽고 사용할 수 있는 방법입니다. 이를 통해 프로그래머는 쉽고 효율적으로 데이터를 처리하고 프로그램을 작성할 수 있습니다.

## 하는 법:

#### 파일 열기
```PHP
<?php
$file = fopen("text.txt", "r") or die("파일을 열 수 없습니다.");
?>
```

#### 파일 읽기
```PHP
<?php
echo fgets($file); // 첫 줄 읽기
echo fread($file,filesize("text.txt")); // 모든 내용 읽기
?>
```

#### 파일 닫기
```PHP
<?php
fclose($file);
?>
```

## 깊이 파보기:

- 역사적 배경: 텍스트 파일은 컴퓨터 시스템에서 매우 유용한 데이터 저장 및 전송 방식으로 오랜 기간 동안 사용되어 왔습니다.
- 대안: 다른 파일 형식을 사용하여 데이터를 저장하고 읽을 수 있지만, 텍스트 파일은 가볍고, 쉽게 읽고 수정할 수 있어 많은 프로그래머들이 선호합니다.
- 구현 상세 정보: fopen(), fread(), fgets() 함수를 사용하여 파일을 열고 읽고 닫을 수 있습니다.

## 참고 자료:

- [PHP fopen() 함수 설명서](https://www.php.net/manual/en/function.fopen.php)
- [PHP fread() 함수 설명서](https://www.php.net/manual/en/function.fread.php)
- [PHP fgets() 함수 설명서](https://www.php.net/manual/en/function.fgets.php)