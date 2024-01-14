---
title:                "PHP: 임시 파일 생성"
simple_title:         "임시 파일 생성"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/php/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## 왜 만들까요?

일시적인 파일을 생성하는 이유는 다양합니다. 예를 들어, 우리는 PHP 프로그래밍에서 종종 파일을 다루거나 읽어야 할 때가 있습니다. 이러한 파일을 생성할 때, 파일이 데이터를 저장하거나 임시적인 작업에 사용될 수 있습니다.

## 만드는 방법

가장 간단한 방법은 `tempnam()` 함수를 사용하는 것입니다. 이 함수는 임시 파일의 이름과 경로를 반환합니다.

```
<?php
$filename = tempnam(sys_get_temp_dir(), "temp");
echo $filename;
?>
```

위의 코드는 현재 시스템의 임시 디렉토리에 "temp"라는 이름의 임시 파일을 생성하고 그 이름을 출력합니다. 아래는 예상되는 출력 결과입니다.

```
/tmp/temp2345
```

또 다른 방법은 `tmpfile()` 함수를 사용하는 것입니다. 이 함수는 임시 파일의 파일 포인터를 반환합니다.

```
<?php
$file = tmpfile();
?>
```

위의 코드는 현재 시스템의 임시 디렉토리에 파일을 생성하고 파일 포인터를 변수에 할당합니다.

## 깊이 살펴보기

우리는 `tempnam()` 함수를 사용할 때, 임시 파일의 이름만 반환되며 파일 자체가 생성되지 않습니다. 따라서 이 파일에 데이터를 저장하려면 `fopen()` 함수를 사용하여 파일을 열어야 합니다.

```
<?php
$filename = tempnam(sys_get_temp_dir(), "temp");
$file = fopen($filename, "w");
fwrite($file, "This is a temporary file.");
fclose($file);
?>
```

위의 코드는 임시 파일을 생성하고 데이터를 입력한 후 파일을 닫습니다.

## 더 알아보기

임시 파일을 생성하는 것은 PHP 프로그래밍에서 자주 사용되는 기능입니다. 그러므로 `tempnam()` 함수와 `tmpfile()` 함수를 잘 익히는 것이 중요합니다. 또한, 임시 파일을 사용하는 다양한 방법들을 더 알아보고 싶다면 아래 링크들을 참고해보세요.

## 더 많은 정보

- [PHP 공식 문서 - `tempnam()` 함수](https://www.php.net/manual/kr/function.tempnam.php)
- [PHP 공식 문서 - `tmpfile()` 함수](https://www.php.net/manual/kr/function.tmpfile.php)
- [w3schools - PHP 파일 관리](https://www.w3schools.com/php/php_file.asp)