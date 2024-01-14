---
title:                "Fish Shell: 컴퓨터 프로그래밍에서 명령 줄 인자 읽기"
simple_title:         "컴퓨터 프로그래밍에서 명령 줄 인자 읽기"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 왜

먼저, 왜 커맨드 라인 인자를 읽는 것이 중요한지 알아보겠습니다. 커맨드 라인 인자를 이용하면 명령어를 더 유연하게 사용할 수 있고 더 많은 기능을 추가할 수 있습니다. 또한 일부 프로그램에서는 주요 기능을 사용할 때 커맨드 라인 인자를 지정해야만 합니다. 이를테면, 파일 이름이나 옵션을 명시하기 위해 커맨드 라인 인자를 사용할 수 있습니다.

## 사용 방법

아래는 Fish Shell에서 커맨드 라인 인자를 읽는 방법에 대한 예제 코드와 출력 결과입니다.

```Fish Shell
#!/usr/bin/fish

# 첫 번째 인자 출력
echo $argv[1]

# 모든 인자 출력
for arg in $argv
    echo $arg
end
```

다음과 같은 명령어를 실행하면:

```bash
./script.fish hello world
```

다음과 같은 출력 결과를 얻을 수 있습니다:

```
hello
hello
world
```

코드에서 볼 수 있듯이, `$argv` 변수를 이용해서 인자를 읽을 수 있습니다. `argv`는 Fish Shell에서 기본적으로 제공되는 변수이며, 모든 커맨드 라인 인자를 담고 있습니다. 이를 이용해서 적절한 위치에 인자를 넣어서 사용할 수 있습니다.

## 깊이 파고들기

Fish Shell에서는 다양한 방법으로 커맨드 라인 인자를 읽을 수 있습니다. 예를 들면, 직접 인자를 지정할 수도 있고, 옵션으로 지정할 수도 있습니다. 또한 인자의 수나 타입 등 다양한 조건을 지정할 수도 있습니다. 이러한 설정 방법은 상황에 따라 유용하게 사용될 수 있으며, 관련 자료나 튜토리얼을 참고하면서 더욱 실력을 향상시킬 수 있습니다.

## 관련 자료 보기

- Fish Shell 공식 문서(https://fishshell.com/docs/current/index.html)
- Linux 커맨드 라인 인자 사용하기(https://www.computerhope.com/issues/ch001935.htm)
- 인프런에서 배우는 Shell Script(https://www.inflearn.com/course/shell-script/)