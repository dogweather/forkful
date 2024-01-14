---
title:    "PHP: 텍스트 파일 읽기"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/php/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 왜

텍스트 파일을 읽는 것이 왜 중요한지 알고 싶으세요? PHP 프로그래밍 웹사이트에 오신 것을 환영합니다! 여러분은 자신이 작성한 프로그램이나 웹사이트에서 사용자가 입력한 데이터를 읽는 등 다양한 상황에서 텍스트 파일을 읽게 될 것입니다. 이 글을 통해 텍스트 파일을 읽는 방법을 배우고 깊이 파고들어보세요.

## 방법

PHP에서 텍스트 파일을 읽는 방법은 간단합니다. "fopen" 함수를 사용하여 파일을 열고 "fgets" 함수를 사용하여 파일로부터 문자열을 읽습니다. 아래에 예시 코드와 출력 결과를 보여드립니다.

```PHP
<?php
$file = fopen("sample.txt", "r"); // "sample.txt" 파일을 읽기 모드로 열기
while(!feof($file)){ // 파일의 끝까지 반복하는 while문 실행
    $line = fgets($file); // 파일로부터 한 줄씩 읽기
    echo $line; // 읽은 문자열 출력
}
fclose($file); // 파일 닫기
?>
```

출력 결과:

```
Hello world!
Welcome to my PHP blog.
```

직접 코드를 작성해보고 다양한 텍스트 파일을 읽어보세요. 또한 "fread" 함수를 사용하여 전체 파일을 한번에 읽어올 수 있습니다. 참고로 fopen 함수로 열기 전에 파일이 존재하는지 체크하는 것이 좋습니다.

## 깊이 파고들기

파일을 읽는 과정에서 에러가 발생할 수 있습니다. PHP에서는 "feof" 함수를 사용하여 파일의 끝인지를 체크하고, 파일의 끝에 도달한 경우 "false"를 반환합니다. 따라서 while문의 조건으로 사용할 수 있습니다. 또한 파일을 열기 전에 "file_exists" 함수를 사용하여 파일이 존재하는지를 체크하는 것이 좋습니다.

## 참고 자료

- PHP 공식 문서 (영문): https://www.php.net/manual/en/function.fopen.php
- 고안 시스템즈 (한글): https://www.gisdeveloper.co.kr/?p=358
- W3Schools (영문): https://www.w3schools.com/php/func_filesystem_fopen.asp