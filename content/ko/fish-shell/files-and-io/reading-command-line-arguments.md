---
title:                "명령줄 인수 읽기"
aliases:
- /ko/fish-shell/reading-command-line-arguments/
date:                  2024-01-20T17:56:10.210738-07:00
model:                 gpt-4-1106-preview
simple_title:         "명령줄 인수 읽기"

tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why? (무엇이며 왜?)
명령줄 인수를 읽기는 스크립트가 사용자 지정 옵션과 데이터를 받는 방법입니다. 프로그래머는 이를 통해 스크립트의 유연성과 사용자 경험을 향상시킵니다.

## How to (어떻게 하나요?)
```Fish Shell
# simple_args.fish
for arg in $argv
    echo "Argument: $arg"
end
```

```sh
$ fish simple_args.fish one two three
Argument: one
Argument: two
Argument: three
```

옵션과 파라미터를 다루기 위한 예제:

```Fish Shell
# options_params.fish
set -l name (status current-command)
set -l age ''
set -l city ''

for arg in $argv
    switch $arg
        case -n --name
            set name $argv[2]
            set argv $argv[2..-1]
        case -a --age
            set age $argv[2]
            set argv $argv[2..-1]
        case -c --city 
            set city $argv[2]
            set argv $argv[2..-1]
        case '*'
            echo "Unknown argument: $arg"
            exit 1
    end
end

echo "Name: $name"
echo "Age: $age"
echo "City: $city"
```

```sh
$ fish options_params.fish --name Alice --age 42 --city Seoul
Name: Alice
Age: 42
City: Seoul
```

## Deep Dive (심도있게 알아보기)
Fish Shell에서는 `$argv` 변수를 이용해 인수를 읽습니다. `$argv`는 리스트 형태로, 스크립트에 전달된 모든 인수를 포함합니다. 다른 셸과 달리 Fish는 내장 `argparse` 명령을 사용하여 옵션 파싱을 제공하지 않습니다. 대신, 전통적인 `for` 루프와 `switch` 문을 사용합니다. 이 방법은 역사적으로 스크립트 언어에서 사용자의 입력을 처리하는데 일반적인 패턴이었습니다. `bash`나 `zsh` 같은 경쟁 셸들은 `getopts` 또는 강력한 `argparse` 구현을 가지고 있지만, Fish의 설계 철학은 간결함과 명확함에 있으므로 복잡한 내장 파싱 도구는 제공하지 않습니다.

## See Also (참고자료)
- [Fish Shell 공식 문서](https://fishshell.com/docs/current/index.html)
- [Command-line arguments in Fish 소개글](https://fishshell.com/docs/current/tutorial.html#tut_scripting)
- [Fish Shell GitHub 저장소](https://github.com/fish-shell/fish-shell)
