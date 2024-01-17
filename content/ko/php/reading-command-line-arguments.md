---
title:                "명령줄 인수 읽기"
html_title:           "PHP: 명령줄 인수 읽기"
simple_title:         "명령줄 인수 읽기"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/php/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
커맨드 라인 인수 읽기는 프로그래머들이 컴퓨터 프로그램에게 사용자로부터 필요한 정보를 전달하는 방법입니다. 이를 통해 사용자는 프로그램 실행 시에 추가적인 설정이나 데이터를 전달할 수 있고, 프로그램도 이를 받아들여 작업을 수행할 수 있게 됩니다.

## 하는 방법:
```PHP
<?php
// 인수를 배열로 받아옵니다.
$arguments = $argv;

// 인수가 존재하는지 확인합니다.
if (count($arguments) > 1) {
  // 첫 번째 인수는 파일명이므로 제외한 후, 두 번째 인수부터 출력합니다.
  for ($i = 1; $i < count($arguments); $i++) {
    echo $arguments[$i] . "\n";
  }
} else {
  // 인수가 없는 경우에는 대신 기본 메시지를 출력합니다.
  echo "아무런 인수가 전달되지 않았습니다.";
}
?>
```

위의 예시 코드를 실행시키면, 커맨드 라인에서 전달한 인수들이 출력됩니다. 만약 아무런 인수도 전달하지 않는다면, 기본 메시지가 출력됩니다.

## 깊이 파고들기:
커맨드 라인 인수 읽기는 오래된 방식의 통신 방법입니다. 하지만 아직도 일부 프로그래밍 환경에서는 매우 유용하게 사용되고 있습니다. 다른 대안으로는 환경 변수를 이용하는 방법이 있지만, 이는 인수보다는 제한적인 정보를 제공할 수 있기 때문에 특정 상황에서는 사용하기 적절하지 않을 수 있습니다.

커맨드 라인 인수 읽기는 PHP 자체에서 지원하며, ```$argv```라는 전역 변수를 통해 인수들을 가져올 수 있습니다. 이 외에도, ```$argc``` 변수를 이용해 전달된 인수의 수를 확인할 수 있습니다. 이를 통해 프로그램에서 적절한 예외 처리를 할 수 있게 됩니다.

## 관련 자료:
- [PHP 공식 문서 - 커맨드 라인 인수 읽기](https://www.php.net/manual/en/features.commandline.php)
- [How to Use Command Line Arguments in PHP](https://www.w3schools.com/php/php_command_line.asp)
- [PHP CLI: How to parse options without external lib?](https://stackoverflow.com/questions/13603011/php-cli-how-to-parse-options-without-external-lib)