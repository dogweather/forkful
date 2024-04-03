---
date: 2024-01-20 17:56:10.210738-07:00
description: "\uBA85\uB839\uC904 \uC778\uC218\uB97C \uC77D\uAE30\uB294 \uC2A4\uD06C\
  \uB9BD\uD2B8\uAC00 \uC0AC\uC6A9\uC790 \uC9C0\uC815 \uC635\uC158\uACFC \uB370\uC774\
  \uD130\uB97C \uBC1B\uB294 \uBC29\uBC95\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\
  \uBA38\uB294 \uC774\uB97C \uD1B5\uD574 \uC2A4\uD06C\uB9BD\uD2B8\uC758 \uC720\uC5F0\
  \uC131\uACFC \uC0AC\uC6A9\uC790 \uACBD\uD5D8\uC744 \uD5A5\uC0C1\uC2DC\uD0B5\uB2C8\
  \uB2E4."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:55.877920-06:00'
model: gpt-4-1106-preview
summary: "\uBA85\uB839\uC904 \uC778\uC218\uB97C \uC77D\uAE30\uB294 \uC2A4\uD06C\uB9BD\
  \uD2B8\uAC00 \uC0AC\uC6A9\uC790 \uC9C0\uC815 \uC635\uC158\uACFC \uB370\uC774\uD130\
  \uB97C \uBC1B\uB294 \uBC29\uBC95\uC785\uB2C8\uB2E4."
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
