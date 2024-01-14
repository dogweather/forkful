---
title:                "Bash: 커맨드 라인 인수 읽기"
simple_title:         "커맨드 라인 인수 읽기"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 왜

커맨드 라인 인자를 읽는 것이 왜 중요한지 궁금하신가요? 우리는 이 블로그 포스트에서 깊이 들어가서 그 이유를 알려드리겠습니다.

## 방법

우선, 커맨드 라인 인자를 읽는 방법에 대해 알아보겠습니다. Bash를 사용하는 경우, `$1`, `$2`과 같은 변수를 사용하여 각각의 인자를 읽을 수 있습니다. 예를 들어, 블로그 포스트 파일명으로 두 개의 인자를 받아 파일명을 출력하는 예시 코드는 다음과 같습니다:

```Bash
#!/bin/bash

echo "첫 번째 인자: $1"
echo "두 번째 인자: $2"
```

위의 예시 코드를 실행해 보면, 파일명과 함께 첫 번째와 두 번째 인자가 출력되는 것을 확인할 수 있습니다. 예시 출력은 다음과 같습니다:

```Bash
$ ./read_arguments.sh bash_post.md blog
첫 번째 인자: bash_post.md
두 번째 인자: blog
```

## 심층 분석

더 깊이 들어가서 커맨드 라인 인자를 읽는 방법을 자세히 살펴보겠습니다. 우선, `$0` 변수는 현재 실행 중인 스크립트의 경로를 나타냅니다. 또한 인자의 개수는 `$#`를 사용하여 확인할 수 있습니다. 또한 `$@` 변수를 사용하면 모든 인자를 배열로 읽을 수 있습니다.

커맨드 라인 인자를 읽는 또 다른 방법은 `getopt` 명령어를 사용하는 것입니다. 이 명령어는 더 유연한 옵션 처리를 위해 사용됩니다. `getopt` 명령어를 사용하는 예시 코드는 다음과 같습니다:

```Bash
#!/bin/bash

options="hf:r:"
blog_file=""
read_flag=""

while getopts "$options" opt; do
  case $opt in
    h)
      echo "도움말: read_arguments.sh -f <파일명> -r (읽기 여부)"
      exit 0
      ;;
    f)
      blog_file=$OPTARG
      ;;
    r)
      read_flag="true"
      ;;
    *)
      exit 1
      ;;
  esac
done

echo "블로그 파일: $blog_file"
echo "읽기 여부: $read_flag"
```

위의 예시 코드에서는 `-h` 옵션으로 도움말을 출력하고, `-f` 옵션으로 파일명을 입력 받고, `-r` 옵션으로 읽기 여부를 지정할 수 있습니다. 예시 출력은 다음과 같습니다:

```Bash
$ ./read_arguments.sh -f bash_post.md -r
블로그 파일: bash_post.md
읽기 여부: true
```

## 또한 보기

- [Bash 스크립트 시작하기](http://bryanwey.com/4.1/shell-scripting-tutorial-bash-beginners/)
- [Bash 스크립트 디버깅하기](http://www.v.veeterzy.com/1.2/shell-script-debugging/)
- [Bash 스크립트 배포하기](http://www.nashvillecalendar.com/2012/11/sharing-shell-scripts-with-community/)
- [GNU `getopt` 메뉴얼](https://www.gnu.org/software/libc/manual/html_node/Argument-Syntax.html)

**Markdown으로 저장해 주세요.**