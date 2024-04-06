---
date: 2024-01-20 17:56:10.210738-07:00
description: "How to (\uC5B4\uB5BB\uAC8C \uD558\uB098\uC694?) Fish Shell\uC5D0\uC11C\
  \uB294 `$argv` \uBCC0\uC218\uB97C \uC774\uC6A9\uD574 \uC778\uC218\uB97C \uC77D\uC2B5\
  \uB2C8\uB2E4. `$argv`\uB294 \uB9AC\uC2A4\uD2B8 \uD615\uD0DC\uB85C, \uC2A4\uD06C\uB9BD\
  \uD2B8\uC5D0 \uC804\uB2EC\uB41C \uBAA8\uB4E0 \uC778\uC218\uB97C \uD3EC\uD568\uD569\
  \uB2C8\uB2E4. \uB2E4\uB978 \uC178\uACFC \uB2EC\uB9AC Fish\uB294 \uB0B4\uC7A5 `argparse`\
  \ \uBA85\uB839\uC744 \uC0AC\uC6A9\uD558\uC5EC \uC635\uC158 \uD30C\uC2F1\uC744 \uC81C\
  \uACF5\uD558\uC9C0\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:57.462936-06:00'
model: gpt-4-1106-preview
summary: "`$argv`\uB294 \uB9AC\uC2A4\uD2B8 \uD615\uD0DC\uB85C, \uC2A4\uD06C\uB9BD\uD2B8\
  \uC5D0 \uC804\uB2EC\uB41C \uBAA8\uB4E0 \uC778\uC218\uB97C \uD3EC\uD568\uD569\uB2C8\
  \uB2E4."
title: "\uBA85\uB839\uC904 \uC778\uC218 \uC77D\uAE30"
weight: 23
---

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
