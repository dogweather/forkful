---
title:    "Fish Shell: 서브스트링 추출하기"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/extracting-substrings.md"
---

{{< edit_this_page >}}

## 왜

서브스트링을 추출하게 되는 이유는 여러 가지가 있을 수 있습니다. 일반적으로 텍스트의 일부분만 필요한 경우, 특정 패턴을 찾아내는 등의 이유로 서브스트링을 추출하게 됩니다.

## 해는

```Fish Shell```에서는 다양한 방법으로 서브스트링을 추출할 수 있습니다.

### 1. ```cut```

```Fish Shell```에는 ```cut```이라는 커맨드가 있습니다. 이를 사용하면 문자열에서 원하는 부분만 추출할 수 있습니다.

예를 들어, 다음과 같은 문자열이 있을 때:

```bash
my_string="안녕하세요, 반가워요"
```

```cut```을 사용하여 "안녕하세요"만 추출할 수 있습니다.

```bash
echo $my_string | cut -d ',' -f1
```

위의 커맨드는 "안녕하세요"를 출력합니다.

### 2. ```grep```

```Fish Shell```에서는 ```grep``` 커맨드를 사용하여 특정 패턴을 찾아내는 것도 가능합니다.

다음과 같은 문자열이 있을 때:

```bash
my_string="abc def 123"
```

```grep```을 사용하여 숫자만 추출할 수 있습니다.

```bash
echo $my_string | grep -o '[0-9]\+'
```

위의 커맨드는 "123"을 출력합니다.

## 딥 다이브

서브스트링을 추출하는 방법은 다양하며, 여러 고급 기능을 사용할 수도 있습니다. 예를 들어, 정규식을 사용하여 문자열에서 특정 패턴을 찾을 수 있고, 특정 위치에서부터 일정 길이의 문자열을 추출할 수도 있습니다.

더 자세한 내용은 [```Fish Shell``` 공식 문서](https://fishshell.com/docs/current/)를 참고하세요.

## 또 보기

- [```cut``` 커맨드 문서](https://fishshell.com/docs/current/cmds/cut.html)
- [```grep``` 커맨드 문서](https://fishshell.com/docs/current/cmds/grep.html)