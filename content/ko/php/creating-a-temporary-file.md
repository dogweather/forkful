---
title:                "임시 파일 생성하기"
html_title:           "Python: 임시 파일 생성하기"
simple_title:         "임시 파일 생성하기"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/php/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

-## 무엇이며 왜 필요한가?-
임시 파일(temp file) 생성이란 네임스페이스가 지정되지 않은 파일을 생성하는 것을 말합니다. 프로그래머들이 이를 활용하는 주요 이유는 큰 데이터를 핸들링하거나, 프로그램 실행 중에 발생하는 데이터를 임시로 저장하기 위함입니다.

-## 어떻게 할 것인가?:-
임시 파일 생성은 tempnam() PHP 함수를 통해 이루어집니다. 이 다음 예제를 보겠습니다:

```PHP
<?php
$tmpfname = tempnam("/tmp", "FOO");

$handle = fopen($tmpfname, "w");
fwrite($handle, "writing to tempfile");
fclose($handle);

$handle = fopen($tmpfname, "r");
$contents = fread($handle, filesize($tmpfname));
fclose($handle);

echo $contents;

unlink($tmpfname);
?>
```

이 스크립트는 "/tmp" 디렉토리에 "FOO" 접두사를 가진 임시 파일을 생성합니다. "writing to tempfile"을 파일에 쓴 후, 파일을 다시 열어 그 내용을 읽습니다.

-## 깊이 파보기:-
PHP의 최초 버전에서 이미 tempnam() 함수가 구현되어 있었다는 사실은, 이 함수의 히스토리컬 맥락을 보여줍니다. PHP에서 임시 파일을 만드는 다른 방법으로는 tmpfile() 함수가 있습니다. 이 함수는 파일 디스크립터와 함께 임시 파일을 생성하고, 스크립트가 종료될 때 해당 파일을 자동으로 삭제합니다. 

하나의 임시 파일을 만드는 방법이 둘 이상일 경우, 가장 이상적인 방법을 선택하는 것이 좋습니다. 이 선택은 주로 사용하려는 데이터의 양, 스크립트 실행 시간, 그리고 스크립트가 완전히 종료된 후에도 파일이 필요한지 여부에 따라 달라집니다.

-## 참고자료:-
1. PHP 공식 문서의 `tempnam()` 함수: https://www.php.net/manual/function.tempnam.php
2. PHP 공식 문서의 `tmpfile()` 함수: https://www.php.net/manual/function.tmpfile.php
3. 임시 파일에 대한 깊은 설명을 포함하기 위한 크롬웹의 PHP 튜토리얼: http://www.cronweb.net/php-tutorial-tmpfile.html