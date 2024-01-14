---
title:                "Fish Shell: 문자열 대문자로 변환하기"
simple_title:         "문자열 대문자로 변환하기"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 왜: 문자열의 첫 글자를 대문자로 바꾸는 것에 관심이 있는 이유

문자열을 대문자로 바꾸는 것은 작업하는 언어에 따라 다른 이유가 있을 수 있지만, 일반적으로 사용자가 원하는 형식에 맞춰서 출력할 때 사용됩니다. 예를 들어, 사용자가 입력한 이름을 출력할 때는 첫 글자를 대문자로 바꾸는 것이 보다 예쁜 출력을 만들 수 있는 방법입니다.

## 사용 방법: "```Fish Shell...```" 코드 블록을 이용한 코딩 예시와 출력 결과

자 이제 여러분도 문자열의 첫 글자를 대문자로 바꾸는 방법을 배우실 차례입니다. 먼저 Fish Shell에서 사용되는 문자열 함수 중 하나인 ```strcap```을 이용하여 첫 글자를 대문자로 바꾸는 방법을 알아보겠습니다.

```
set name "john"
echo (string match -r '[a-z]+' $name ? (string match -r '[a-z]+' $name | string sub -s 1)"cmatch $name | string sub -s 1)
```

위의 코드 블록을 실행하면 출력 결과는 "John"이 될 것입니다.

## 깊이 들어가보기: 문자열 중간에 있는 글자를 대문자로 바꾸기

지금까지 우리는 문자열의 첫 글자를 대문자로 바꾸는 방법만을 알아보았습니다. 하지만 만약 문자열 중간에 있는 특정 글자를 대문자로 바꾸고 싶다면 어떻게 해야 할까요? 이 때 사용할 수 있는 기능이 있습니다. 바로 ```strupper``` 함수입니다. 이 함수는 문자열의 모든 글자를 대문자로 바꾸어주는 기능을 합니다. 따라서, 원하는 위치의 글자만 대문자로 바꾸어주기 위해서는 해당 위치의 문자를 대문자로 바꾼 다음, 나머지 글자는 다시 소문자로 바꿔주면 됩니다.

예를 들어, 문자열 "johnSmith"에서 6번째 위치에 있는 "S"를 대문자로 바꾸고 싶다면 다음과 같이 코드를 작성할 수 있습니다.

```
set name "johnSmith"
set upper (string -s 6 $name | string upcase)
set result (string sub -s 6 $name)
echo $result$upper
```

위의 코드를 실행하면 출력 결과는 "johnSmitH"가 될 것입니다.

## 참고 자료

- [Fish Shell 문서](https://fishshell.com/docs/current/commands.html)
- [유용한 Fish Shell 스크립트 코드](https://github.com/jorgebucaran/fish-shell-cookbook)
- [자바스크립트에서 문자열의 첫 글자를 대문자로 바꾸는 방법](https://www.w3schools.com/jsref/jsref_touppercase.asp)

## 참고하기

[비쥬얼 스튜디오 코드에서 Fish Shell 설정하기](https://marketplace.visualstudio.com/items?itemName=mcneelco.Fish)