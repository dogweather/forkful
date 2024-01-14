---
title:    "Fish Shell: 텍스트 파일 읽기"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 왜

텍스트 파일을 읽는 방법에 대해 더 배우고 싶은 독자를 위해 이 블로그를 쓴 것입니다. 이글은 Fish Shell 프로그래밍 언어를 활용하여 텍스트 파일을 읽는 방법에 대한 샘플 코드와 실제 출력을 보여줍니다.

## 어떻게

Fish Shell을 사용하여 텍스트 파일을 읽는 것은 매우 쉽습니다. 우선 텍스트 파일을 콘솔 창에서 열고 다음과 같은 명령을 입력하면 됩니다.

```Fish Shell
cat [텍스트 파일 이름]
```

위 명령어를 실행하면 파일의 내용이 콘솔 창에 출력됩니다.

만약 여러 개의 파일을 동시에 읽고 싶다면 아래와 같은 코드를 사용할 수 있습니다.

```Fish Shell
cat [파일1 이름] [파일2 이름] [파일3 이름]
```

또는 Wildcard 문자를 사용하여 여러 파일을 선택할 수도 있습니다.

```Fish Shell
cat *.txt
```

## 심층 분석

보다 깊게 들어가서 텍스트 파일을 읽는 방법을 살펴보겠습니다. Fish Shell의 경우 `cat` 명령어 대신 `read`를 사용하여 파일을 읽을 수 있습니다. 예를 들어, `read`를 사용하여 콘솔 창에서 사용자로부터 텍스트를 입력 받아 파일이름.txt 파일에 저장하는 코드는 다음과 같습니다.

```Fish Shell
echo "어서오세요, 파일에 저장할 텍스트를 입력하세요."
set text (read)
echo $text > 파일이름.txt
```

만약 파일의 내용을 바로 변수에 저장하고 싶다면 `set` 명령어를 사용할 수 있습니다.

```Fish Shell
set content (cat 파일이름.txt)
```

이렇게 하면 변수 `content`에 파일의 내용이 저장됩니다.

## 참고 자료

- [Fish Shell 공식 홈페이지](https://fishshell.com/)
- [Fish Shell GitHub 저장소](https://github.com/fish-shell/fish-shell)
- [Fish Shell 사용자 매뉴얼](https://fishshell.com/docs/current/index.html)