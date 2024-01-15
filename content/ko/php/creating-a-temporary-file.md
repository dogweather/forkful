---
title:                "임시 파일 만들기"
html_title:           "PHP: 임시 파일 만들기"
simple_title:         "임시 파일 만들기"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/php/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## 왜

임시 파일을 만드는 이유는 여러 가지가 있습니다. 예를 들어, 프로세스나 프로그램 실행 중에 필요한 임시 데이터를 저장하거나, 다운로드한 파일을 임시적으로 저장하는 경우 등이 있을 수 있습니다. 이를 통해 시스템 리소스를 효율적으로 관리하고, 프로그램의 실행 속도를 빠르게 할 수 있습니다.

## 만드는 방법

임시 파일을 만드는 방법은 간단합니다. PHP에서는 `tmpfile()` 함수를 사용하여 새로운 임시 파일을 생성할 수 있습니다. 이 함수는 임시 파일의 핸들을 반환하므로, 파일에 접근하거나 작업을 수행할 수 있습니다. 아래 코드를 참고해 보세요.

```PHP
<?php
$file = tmpfile();
if ($file) {
  // 임시 파일에 데이터 쓰기
  fwrite($file, "Lorem ipsum dolor sit amet, consectetur adipiscing elit.");
  // 임시 파일의 데이터 읽기
  echo fread($file, 256);
  // 임시 파일 삭제
  fclose($file);
}
```

위 코드는 다음과 같은 결과를 출력합니다.

```
Lorem ipsum dolor sit amet, consectetur adipiscing elit.
```

## 깊게 파헤쳐보기

`tmpfile()` 함수는 실제로는 시스템의 `tmp` 디렉토리에 임시 파일을 생성하고, 해당 파일을 연결해서 리소스를 반환하는 것입니다. 따라서 임시 파일의 이름이나 경로를 직접 지정할 수는 없습니다. 만약 임시 파일의 이름이나 경로가 필요하다면, `tempnam()` 함수를 사용하여 생성할 수 있습니다. 이 함수는 임시 파일의 경로를 지정할 수 있고, 파일의 핸들 대신 파일 이름을 반환합니다.

임시 파일을 사용한 후에는 `fclose()` 함수를 사용하여 파일을 닫아야 합니다. 이렇게 하면 임시 파일은 자동으로 삭제됩니다. 또는 `unlink()` 함수를 사용하여 파일을 직접 삭제할 수도 있습니다. 임시 파일의 사용이 끝나면 즉시 삭제하는 것이 좋은 습관입니다.

## 관련 자료

- [PHP tmpfile() 함수 문서](https://www.php.net/manual/en/function.tmpfile.php)
- [PHP tempnam() 함수 문서](https://www.php.net/manual/en/function.tempnam.php)
- [PHP fopen() 함수 문서](https://www.php.net/manual/en/function.fopen.php)

See Also

[PHP 공식 문서](https://www.php.net/manual/en/)