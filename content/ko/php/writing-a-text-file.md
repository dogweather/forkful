---
title:                "PHP: 텍스트 파일 작성"
simple_title:         "텍스트 파일 작성"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/php/writing-a-text-file.md"
---

{{< edit_this_page >}}

# 왜
 
텍스트 파일을 작성하는 이유는 다양합니다. 예를 들어, 서버에서 사용자의 입력 데이터를 저장하거나 파일의 내용을 읽어서 웹 페이지에 표시하는 등의 용도로 사용할 수 있습니다.

# 하는 방법

```PHP
<?php
// 파일을 쓰기 모드로 열어서 변수에 저장합니다.
$file = fopen("example.txt", "w");

// 변수에 저장된 내용을 파일에 씁니다.
fwrite($file, "Hello World!");

// 파일을 닫습니다.
fclose($file);
?>
```

위 예제에서는 "example.txt"라는 이름의 파일을 생성하고, 그 안에 "Hello World!"라는 텍스트를 씁니다.

# 깊게 파헤치기

파일을 생성하고 내용을 쓰는 것 이외에도, 파일을 읽어서 변수에 저장하는 등 다양한 방법으로 텍스트 파일을 다룰 수 있습니다. 또한 파일을 열 때 옵션으로 "a"를 전달하면 기존의 파일 끝에 내용을 추가할 수도 있습니다.

# 자세히 알아보기

PHP에서 파일을 다루는 방법에는 여러 가지가 있으며, 파일의 크기가 크다면 성능을 고려할 때 다른 방법이 필요할 수도 있습니다. 따라서 필요에 따라 다양한 방법을 선택하여 적절하게 사용하는 것이 중요합니다.

# 참고자료

- [PHP: fopen - Manual](https://www.php.net/manual/en/function.fopen.php)
- [PHP: fwrite - Manual](https://www.php.net/manual/en/function.fwrite.php)
- [PHP: fclose - Manual](https://www.php.net/manual/en/function.fclose.php)