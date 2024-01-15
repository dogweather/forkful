---
title:                "임시 파일 만들기"
html_title:           "Fish Shell: 임시 파일 만들기"
simple_title:         "임시 파일 만들기"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## 왜

여러분들이 임시 파일을 만드는 방법을 배우는 이유는 간단합니다. 간혹 특정 작업을 할 때, 임시적이고 일시적으로 정보를 저장해야 할 때가 있습니다. 이때 임시 파일을 사용하면 우리가 필요한 정보를 효율적으로 저장하고, 프로그램이 종료되면 자동으로 삭제되기 때문에 공간 절약과 보안 측면에서도 유용합니다.

## 해보기전에

먼저, Fish Shell을 사용하기 위해 설치가 되어있어야 합니다. 아래의 명령어를 사용하여 설치할 수 있습니다.

```
설치 명령어 입력 예: brew install fish
```

임시 파일을 만드는 가장 간단한 방법은 다음과 같습니다.

```
Fish Shell 예제 입력:
set temp_file (mktemp)
echo "임시 파일 텍스트" > $temp_file
echo $temp_file
```

위 코드를 실행하면 해당 디렉토리에 임시 파일이 생성되고, 텍스트가 쓰여진 후 해당 파일의 경로가 출력됩니다. 다음으로는 임시 파일을 삭제하는 방법입니다.

```
Fish Shell 예제 입력:
rm $temp_file
ls $temp_file
```

`rm` 명령어를 사용하여 임시 파일을 삭제하고, 다시 `ls` 명령어를 사용하여 해당 파일이 삭제되었는지 확인할 수 있습니다.

## 더 깊게

위의 예시 코드는 기본적인 임시 파일 생성과 삭제 방법을 보여주기 위한 것입니다. 하지만 실제로 프로그래밍에서 임시 파일을 생성하는 데에는 여러 옵션이 존재합니다. 예를 들어, 파일의 이름을 지정하거나, 특정 디렉토리에 생성하는 등의 다양한 방식이 있습니다. 자세한 내용은 Fish Shell 공식 문서를 참고하시기 바랍니다. 또한 임시 파일 생성 외에도, 임시 디렉토리를 생성하는 `mktemp -d` 명령어도 있으니 참고해보시기 바랍니다.

## 처음이시라면

에디터와 터미널을 다루는 데에 익숙한 분들이라면 누구나 쉽게 실습해볼 수 있을 것입니다. 하지만 익숙하지 않다면, 아래의 링크를 참고하여 Fish Shell과 기본적인 터미널 명령어를 익힐 수 있습니다.

[Learning Fish Shell](https://fishshell.com/docs/current/index.html)

## 유용한 링크

- [Fish Shell 공식 문서](https://fishshell.com/docs/current/index.html)
- [Brew로 Fish Shell 설치하기](https://formulae.brew.sh/formula/fish)
- [mktemp 명령어 설명서](https://linux.die.net/man/1/mktemp)
- [Learn Shell in 10 Minutes](https://www.codecademy.com/learn/learn-the-command-line/modules/learn-the-command-line-navigation/cheatsheet)
- [Terminus: Fish Shell Live Tutorial](https://www.youtube.com/watch?v=F_gznforSE0)