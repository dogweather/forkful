---
title:                "표준 오류에 쓰는 방법"
html_title:           "PHP: 표준 오류에 쓰는 방법"
simple_title:         "표준 오류에 쓰는 방법"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/php/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# 왜

**왜** 누군가가 표준 오류에 쓰기를 진행할까요?

**표준 오류**는 유용한 디버깅 도구입니다. 코드를 실행하는 과정에서 발생한 오류 메시지와 함께 디버깅 정보를 제공해줍니다. 이를 통해 개발자들은 코드를 더욱 안정적이고 정확하게 만들 수 있습니다.

# 어떻게

이제 표준 오류에 쓰는 방법을 배워보겠습니다. 아래의 코드 블록은 PHP를 사용하여 표준 오류에 메시지를 쓰는 간단한 예제입니다.

```PHP
<?php
$error_message = "작동 중지!";
fwrite(STDERR, $error_message);
?>
```

위 코드를 실행하면 "작동 중지!"라는 오류 메시지가 표준 오류에 쓰여집니다.

코드를 조금 더 자세히 살펴보면, `fwrite` 함수를 사용하여 오류 메시지를 출력하고 있습니다. 첫 번째 인자로는 표준 오류를 나타내는 `STDERR`을 전달하고, 두 번째 인자로는 출력할 메시지인 `$error_message`를 전달합니다.

# 깊이 살펴보기

표준 오류에 쓰기는 디버깅에 유용한 도구일 뿐만 아니라 예외 처리에도 활용될 수 있습니다. 예를 들어, 특정 조건에서 코드가 예상대로 작동하지 않을 때 예외를 발생 시키고, 표준 오류에 해당 예외를 쓰는 것이 가능합니다.

또한 표준 오류에 쓰기는 로그 기능으로도 사용될 수 있습니다. 실행 중인 코드에서 중요한 정보를 기록하고 싶을 때 표준 오류에 내용을 쓰면, 이를 추후에 확인할 수 있습니다.

# 관련 링크

* [PHP fwrite 함수의 공식 문서](https://www.php.net/manual/en/function.fwrite.php)
* [PHP 표준 오류를 다루는 공식 문서](https://www.php.net/manual/en/features.commandline.io-streams.php)
* [PHP 오류 처리 가이드](https://www.php.net/manual/en/language.exceptions.php)

---

# 관련 링크

* [Markdown 이해](https://www.markdownguide.org/)
* [PHP 공식 사이트](https://www.php.net/)
* [PHP에 대한 더 많은 자료](https://www.phptherightway.com/)