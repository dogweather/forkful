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

## 왜
언어에 대한 지식이 없어도 웹 개발을 할 수 있기 때문에 많은 사람들이 PHP에 관심을 갖습니다. PHP는 유연하고 쉽고, 사용하기 쉬우며, 웹 개발에 필수적인 기능들을 제공해줍니다. 따라서 PHP의 기능을 최대한 활용하기 위해서는 PHP의 기본 개념들을 이해하고 있어야 합니다. 이 글에서는 PHP에서 텍스트 파일을 읽는 방법에 대해 알아보도록 하겠습니다.

## 어떻게
### PHP에서 텍스트 파일 읽기
PHP에서 텍스트 파일을 읽는 방법은 다음과 같이 간단합니다.
```PHP
$myfile = fopen("example.txt", "r") or die("Unable to open file!");
echo fread($myfile,filesize("example.txt"));
fclose($myfile);
```
위의 코드는 `fopen()` 함수를 사용하여 `example.txt` 파일을 열고, `fread()` 함수를 사용하여 파일의 내용을 읽은 후에 `fclose()` 함수를 사용하여 파일을 닫습니다.

### 예제
예를 들어, `example.txt` 파일에 다음과 같은 내용이 있다고 가정해봅시다.
```
Hello, world!
This is a text file.
```
위의 코드를 실행하면 다음과 같은 결과가 출력될 것입니다.
```
Hello, world!
This is a text file.
```

## 깊이 파고들기
### fopen() 함수
PHP에서 텍스트 파일을 읽기 위해서는 `fopen()` 함수를 사용해야 합니다. 이 함수는 파일을 열고 파일의 핸들을 반환합니다. 핸들은 파일의 모든 작업을 컨트롤하는데 사용됩니다. 또한 `fopen()` 함수의 두 번째 매개변수로 파일을 읽는 모드를 지정할 수 있습니다. `r` 모드는 파일을 읽기 전용으로 열게 됩니다.

### fread() 함수
`fread()` 함수는 지정된 파일에서 지정된 바이트수만큼 읽어옵니다. 위의 예제에서는 `filesize()` 함수를 사용하여 파일의 크기를 구했지만, 이 값을 직접 지정해줘도 상관 없습니다. `fread()` 함수의 두 번째 매개변수로 읽어올 바이트 수를 지정해주면 됩니다.

### fclose() 함수
파일을 읽은 후에는 `fclose()` 함수를 사용하여 파일을 닫아주는 것이 좋습니다. 이는 운영 체제에게 파일 사용이 끝났다는 신호를 보내는 것과 같습니다. 이를 통해 불필요한 자원 낭비를 방지할 수 있습니다.

## See Also
- [PHP 소개](https://www.php.net/manual/en/intro-whatis.php)
- [PHP 열기/읽기/닫기](https://www.php.net/manual/en/function.fopen.php)
- [PHP 파일 읽기](https://www.w3schools.com/php/php_file_open.asp)