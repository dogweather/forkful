---
title:                "Fish Shell: 텍스트 파일 읽기"
simple_title:         "텍스트 파일 읽기"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 왜
이번 포스트에서는 텍스트 파일을 읽는 방법에 대해 소개하겠습니다. 이 글을 읽는 이유는 텍스트 파일을 읽는 것이 프로그래밍에서 중요한 기술이기 때문입니다. 간단한 예제와 함께 텍스트 파일을 읽는 방법을 알아보겠습니다.

## 방법
먼저, 텍스트 파일을 읽는 가장 간단한 방법은 Fish Shell의 `cat` 명령어를 사용하는 것입니다. `cat` 명령어는 파일의 내용을 터미널에 출력해줍니다. 아래의 예제를 통해 `cat` 명령어를 어떻게 사용하는지 살펴보겠습니다.

```Fish Shell
cat hello.txt
```

위의 명령어는 `hello.txt` 파일의 내용을 출력합니다. 만약 파일이 존재하지 않는다면 에러가 발생합니다. 이번에는 파일의 내용을 `>` 기호를 사용하여 다른 파일에 저장하는 방법을 살펴보겠습니다.

```Fish Shell
cat hello.txt > output.txt
```

위의 명령어는 `hello.txt` 파일의 내용을 `output.txt` 파일에 저장합니다. `output.txt` 파일이 이미 존재한다면 덮어쓰기가 되니 주의해야 합니다. 또 다른 방법으로는 파일의 내용을 `>>` 기호를 사용하여 기존 파일에 추가하는 방법이 있습니다.

```Fish Shell
cat hello.txt >> output.txt
```

위의 명령어는 `hello.txt` 파일의 내용을 `output.txt` 파일의 끝에 추가합니다. 이제 텍스트 파일을 읽는 데 기본적인 방법은 알았으니 조금 더 심화된 내용을 살펴보겠습니다.

## 깊이 있는 내용
텍스트 파일을 읽을 때 주의할 점은 파일의 크기입니다. 만약 파일이 매우 크다면 `cat` 명령어를 사용할 때 많은 양의 데이터가 터미널에 출력되기 때문에 사용하기 불편할 수 있습니다. 이럴 때는 `less` 명령어를 사용하면 좋습니다. `less` 명령어는 파일의 내용을 한 페이지씩 터미널에 출력해줍니다.

```Fish Shell
less huge_file.txt
```

여러분은 페이지를 넘기는 방법을 알고 있을 겁니다. `less` 명령어를 나가려면 `q` 키를 누르면 됩니다. 이 외에도 `grep`이나 `sed`와 같은 명령어를 이용하면 더욱 다양한 작업을 할 수 있습니다. 이번 포스트에서는 다루지 않았지만 추가적인 방법을 알아보고 싶다면 아래의 링크를 참고하시기 바랍니다.

## 또 다른 방법
- [Fish Shell 공식 문서](https://fishshell.com/docs/current/tutorial.html#tut_pipes_and_redirections)
- [Unix 명령어 공부하기](https://ss64.com/bash/)