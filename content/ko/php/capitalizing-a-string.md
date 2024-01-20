---
title:                "문자열 대문자화"
html_title:           "PHP: 문자열 대문자화"
simple_title:         "문자열 대문자화"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/php/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 무엇이고 왜?
문자열의 첫 문자를 대문자로 만드는 것을 'capitalizing a string'이라고 합니다. 이렇게 하는 이유는 다양한데, 특히 이름이나 제목 등의 경우 첫 글자를 대문자로 만들어 사용자에게 제공하려는 경우가 많습니다.

## 사용 방법:
다음은 PHP에서 문자열 첫 글자를 대문자로 만드는 방법을 보여주는 코드입니다.

```PHP
<?php
$my_str = 'hello, world!';
$my_str = ucfirst($my_str);
echo $my_str;
?>
```

이 코드를 실행하면, 다음과 같은 결과를 얻을 수 있습니다.

```PHP
Hello, world!
```

## 심층적인 정보:
문자열의 첫 문자를 대문자로 만드는 것은 오래 전부터 이루어져 왔습니다. 이를 수행하는 다양한 방법이 있지만 PHP에서는 ucfirst 함수를 사용하는 것이 가장 간단합니다.

또한, 문자열의 모든 단어의 첫 문자를 대문자로 만들고 싶다면 ucwords 함수를 사용할 수 있습니다.

```PHP
<?php
$my_str = 'hello, world!';
$my_str = ucwords($my_str);
echo $my_str;
?>
```    
위 코드를 사용하면 결과는 ‘Hello, World!’가 됩니다.

## 참고 자료:
다음은 대문자로 만드는 함수에 대한 PHP 공식 문서 링크입니다.
- ucfirst(): [https://www.php.net/manual/function.ucfirst.php](https://www.php.net/manual/function.ucfirst.php)
- ucwords(): [https://www.php.net/manual/function.ucwords.php](https://www.php.net/manual/function.ucwords.php)