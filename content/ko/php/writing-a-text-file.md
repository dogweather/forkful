---
title:                "텍스트 파일 작성하기"
html_title:           "Arduino: 텍스트 파일 작성하기"
simple_title:         "텍스트 파일 작성하기"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/php/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why? (무엇이며 왜?)
텍스트 파일 쓰기는 문자 데이터를 파일 형태로 저장하는 것입니다. 프로그래머는 로그 저장, 설정 관리, 데이터 교환 등을 위해 이를 사용합니다.

## How to: (방법)
```php
<?php
// 파일 쓰기 준비
$filename = "example.txt";
$content = "안녕하세요, PHP 파일 쓰기 예제입니다.";

// 파일 쓰기 - 파일이 없으면 생성
file_put_contents($filename, $content);

// 파일 추가 쓰기 - 기존 내용을 유지하고 뒤에 추가 
$additionalContent = "\n새로운 내용 추가!";
file_put_contents($filename, $additionalContent, FILE_APPEND);

// 결과 확인: 파일 내용을 읽어 출력
echo file_get_contents($filename);
?>
```
Sample Output:
```
안녕하세요, PHP 파일 쓰기 예제입니다.
새로운 내용 추가!
```

## Deep Dive (심층 분석)
과거에는 `fopen()`, `fwrite()`, 그리고 `fclose()` 함수를 사용하여 파일을 썼습니다. `file_put_contents()`는 이러한 과정을 단축시켜 줍니다. 또한, 파일 쓰기에 사용되는 함수들은 파일 시스템 권한과 충돌할 수 있으니 유의해야 합니다. 텍스트 파일 대신 데이터베이스를 사용할 수도 있으나, 단순성과 접근성 때문에 여전히 파일 쓰기는 유용합니다.

## See Also (더 보기)
- [PHP 공식 문서: file_put_contents()](https://www.php.net/manual/en/function.file-put-contents.php)
- [PHP 공식 문서: fopen()](https://www.php.net/manual/en/function.fopen.php)
- [PHP 파일 시스템 권한](http://php.net/manual/en/features.file-upload.errors.php)
- [PHP 스트림 문서](https://www.php.net/manual/en/book.stream.php)