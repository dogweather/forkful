---
title:                "임시 파일 생성하기"
html_title:           "Bash: 임시 파일 생성하기"
simple_title:         "임시 파일 생성하기"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## 왜
우리는 종종 일시적인 파일을 생성해야 할 때가 있습니다. 예를 들어, 프로그램 실행 중 생성되는 임시 파일이 필요하거나 특정 작업을 수행하기 위해 임시 파일을 만들어야 할 때가 있습니다. 따라서 Bash를 사용하여 임시 파일을 생성하는 방법을 배우면 효율적으로 프로그래밍할 수 있습니다.

## 어떻게
"mktemp" 명령어를 사용하여 임시 파일을 생성할 수 있습니다. "파일 이름 + 임의의 숫자 뒤에 붙는 6자리 임시 이름" 형식으로 파일이 생성됩니다.

```Bash
# mktemp - 생성된 임시 파일의 경로를 출력
$ mktemp
/tmp/tmp.WbeLhb

# 임의의 디렉토리에 임시 파일 생성
$ mktemp -p ~/Desktop/
~/Desktop/tmp.E9WvM7
```

또한 "tempfile" 함수를 사용하여 임시 파일을 생성할 수도 있습니다. 이 함수는 "FILE" 변수에 임시 파일의 경로를 할당합니다.

```Bash
#!/bin/bash
TEMP=$(tempfile)
echo "임시 파일 경로: $TEMP"
# 임시 파일 경로: /tmp/tmp.0Qu3eo
```

각 임시 파일은 고유한 이름을 가지고 있으며 프로그램이 종료될 때 자동으로 삭제됩니다.

## 깊이 들어가기
임시 파일을 생성하기 전에 우리는 "trap" 명령어를 사용하여 프로그램이 종료될 때 임시 파일을 자동으로 삭제할 수 있도록 설정할 수 있습니다. 또한 임시 파일을 생성할 때 속성을 지정하여 다양한 용도로 사용할 수 있습니다.

## 참고 
- [MKTEMP man 페이지]
- [TEMPFILE man 페이지]

[MKTEMP man 페이지]: https://www.systutorials.com/docs/linux/man/1-mktemp/
[TEMPFILE man 페이지]: https://man7.org/linux/man-pages/man3/tempfile.3.html