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

## 무엇 & 왜?
표준 오류에 쓰기란 무엇이며, 왜 프로그래머들이 이를 사용하는지에 대해 두 세 문장으로 설명하겠습니다.

## 방법:
아래의 ```PHP ... ``` 코드 블록 안에 코딩 예시와 샘플 출력을 포함하였습니다.

```
<?php
$name = "John";
fwrite(STDERR, "Hello, $name");
```

출력:
```
Hello, John
```

## 자세히 알아보기:
표준 오류에 쓰기에 대한 역사적 배경, 대안들, 그리고 구현 세부 사항 등에 대해 알아보겠습니다.

### 역사적 배경:
표준 오류에 쓰기는 초기 컴퓨터 시스템에서부터 사용되어온 기능입니다. 보통은 디버깅을 위해 사용되었으며, 프로그램의 오류를 신속하게 발견하고 수정하는 데 도움이 되기도 했습니다.

### 대안:
표준 오류에 쓰기 대신 사용할 수 있는 다른 방법으로는 로그 파일 작성이 있습니다. 로그 파일은 실행 중인 프로그램의 모든 정보를 기록하며, 오류가 발생했을 때 해당 내용을 추적하는 데 유용합니다.

### 구현 세부 사항:
표준 오류에 쓰기는 간단한 함수인 fwrite()를 사용하여 구현할 수 있습니다. 이 함수는 첫 번째 인자로 파일 포인터를, 두 번째 인자로 출력할 내용을 받습니다.

## 참고:
관련 자료를 참고할 수 있는 링크입니다.

- [PHP fwrite() 함수 문서](https://www.php.net/manual/en/function.fwrite.php)