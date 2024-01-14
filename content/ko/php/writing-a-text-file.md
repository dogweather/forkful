---
title:    "PHP: 텍스트 파일 쓰기"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/php/writing-a-text-file.md"
---

{{< edit_this_page >}}

## 왜

텍스트 파일을 작성하려면 무엇을해야합니까? 프로그래밍에서 텍스트 파일은 중요한 역할을합니다. 예를 들어, 데이터를 저장하고 읽는 데 사용될 수 있습니다. 또는 웹 사이트에 동적 콘텐츠를 생성하는 데도 사용할 수 있습니다.

## 어떻게

```PHP
<?php

// 파일을 쓰기 모드로 엽니다.
$myfile = fopen("textfile.txt", "w") or die("파일을 열 수 없습니다!");

// 파일에 데이터를 씁니다.
$txt = "안녕하세요, PHP 프로그래밍을 배워보세요!";
fwrite($myfile, $txt);

// 파일을 닫습니다.
fclose($myfile);

// 파일을 읽기 모드로 엽니다.
$myfile = fopen("textfile.txt", "r") or die("파일을 열 수 없습니다!");

// 파일의 내용을 읽어옵니다.
echo fgets($myfile);

// 파일을 닫습니다.
fclose($myfile);
?>
```

위의 예제는 "textfile.txt"라는 텍스트 파일을 생성하고 거기에 "안녕하세요, PHP 프로그래밍을 배워보세요!"라는 문구를 쓴 다음 다시 읽어오는 과정을 보여줍니다. 각각의 코드 블록은 백틱 기호를 사용하여 구분됩니다.

## 딥 다이브

파일을 생성하고 데이터를 쓰고 읽는 것은 프로그래밍에서 자주 사용되는 작업입니다. 그러나 실제로 파일을 작성하는 것은 더 복잡할 수 있습니다. 텍스트 파일을 작성하는 데 필요한 요소들은 파일의 이름, 저장 위치, 데이터의 형식 등이 있습니다. 또한 파일을 읽고 쓰는 데 있어서 파일의 크기나 보안 등의 이유로 추가적인 처리가 필요할 수 있습니다. 이러한 디테일한 내용들은 프로그래머들에게 중요한 공부주제가 될 수 있습니다.

## 또 다른 자료들

- [PHP 파일 입출력 - w3schools](https://www.w3schools.com/php/php_file_open.asp)
- [파일을 읽고 쓰는 방법 - PHP.net](https://www.php.net/manual/en/function.fopen.php)
- [파일 읽고 쓰기 관련 함수 - TutorialsPoint](https://www.tutorialspoint.com/php/php_file_inclusion.htm)
- [파일 입력받기 - Programiz](https://www.programiz.com/php-programming/file-input)