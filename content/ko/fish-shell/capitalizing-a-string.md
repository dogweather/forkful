---
title:    "Fish Shell: 문자열 대문자화하기"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# 왜

자바스크립트에서 소문자로 된 문자열을 대문자로 바꿔야 할 때가 있습니다. 예를 들어, 사용자의 이름을 받아서 화면에 출력할 때, 대문자로 보여주면 더 극락적입니다. 이런 변환을 도와주는 간단한 방법을 알아봅시다.

## 코딩하는 방법

먼저, Fish Shell에서 `string` 명령어를 이용하여 원하는 문자열을 입력합니다. 그 다음에는 `tr` 명령어를 사용하여 소문자를 대문자로 변환합니다. 예제 코드는 다음과 같습니다:

```Fish Shell
string name = "john"
echo $name | tr '[a-z]' '[A-Z]'
```

이 코드를 실행하면 `JOHN`이라는 출력값을 볼 수 있습니다.

## 더 깊이 들어가기

하지만 `tr` 명령어는 ASCII 문자만 변환해주는 것을 유의해야 합니다. 만약 한국어 문자열을 변환하려면, `tr` 명령어만으로는 충분하지 않습니다. 이럴 때는 `sed` 명령어를 사용하여 대체할 수 있습니다. 예를 들어, 한국어 이름인 "김치"를 대문자로 바꾸는 코드는 다음과 같습니다:

```Fish Shell
string name = "김치"
echo $name | sed 's/\(.\)/\U\1/g'
```

출력값은 `김치`가 아닌 `김칠`로 나오게 됩니다. 이유는 `김칠`의 ASCII 코드가 `김치`보다 더 크기 때문입니다.

## 더 많은 참고자료

[`tr` 명령어 추가 정보](https://fishshell.com/docs/current/commands.html#tr)

[`sed` 명령어 추가 정보](https://fishshell.com/docs/current/commands.html#sed)

["Fish Shell을 사용해보세요" 블로그 포스트](https://sujinlee.me/fish-shell/)