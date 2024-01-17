---
title:                "text 파일 쓰기"
html_title:           "PHP: text 파일 쓰기"
simple_title:         "text 파일 쓰기"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/php/writing-a-text-file.md"
---

{{< edit_this_page >}}

## 무엇인지 & 왜 하는가?

텍스트 파일을 작성하는 것은 프로그래머가 파일에 텍스트 정보를 저장하기 위해 사용하는 것입니다. 예를 들어, 사용자 데이터나 로그 파일 등을 저장할 수 있습니다. 이것은 프로그래머가 읽고 쓸 수 있도록 데이터를 영구적으로 저장하는 방법입니다.

## 방법:

```php
$myfile = fopen("newfile.txt", "w"); // 새 파일 생성
$txt = "Hello world!"; // 텍스트 지정
fwrite($myfile, $txt); // 파일에 텍스트 쓰기
fclose($myfile); // 파일 닫기
```
위 코드를 실행하면 "newfile.txt"라는 텍스트 파일이 생성되고, 파일 안에 "Hello world!"가 쓰여집니다.

## 더 깊이 들어가보기:

1. 텍스트 파일은 컴퓨터에서 가장 오래된 형태의 데이터 저장 방식입니다. 따라서 프로그래머들이 텍스트 파일을 사용하는 것은 역사적으로 자연스러운 일입니다.

2. 텍스트 파일 외에도 데이터를 저장하는 다른 방식으로는 데이터베이스가 있습니다. 데이터베이스는 데이터를 더 구조화해서 관리할 수 있지만, 텍스트 파일은 간단하고 빠르게 사용할 수 있습니다.

3. fopen 함수에 대한 자세한 정보나 파일 모드에 대한 다른 옵션들을 알고 싶다면 [공식 PHP 문서](https://www.php.net/manual/en/function.fopen.php)를 참고해주세요.

## 참고 자료:

1. [PHP에서 파일 읽고 쓰기](https://opentutorials.org/course/62/5127) 강좌에서 더 많은 예제 코드와 실습을 할 수 있습니다.

2. PHP가 아닌 다른 언어로 파일을 읽고 쓰는 방법을 배우기 위해 [w3schools - 파일 처리](https://www.w3schools.com/php/php_file_open.asp)를 참고해보세요.

3. [PHP 파일 처리 부분](https://www.php.net/manual/en/ref.filesystem.php)에서 PHP의 파일 처리 관련 함수들을 자세히 알아볼 수 있습니다.