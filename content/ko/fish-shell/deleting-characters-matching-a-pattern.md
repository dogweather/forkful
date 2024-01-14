---
title:                "Fish Shell: 패턴과 일치하는 문자 삭제"
programming_language: "Fish Shell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# 왜
패턴에 맞는 문자를 삭제하는 것에 참여하는 이유는 파일이나 문자열에서 쓸모 없는 정보를 정리하고 데이터를 정제하는 것입니다.

## 사용 방법 
```Fish Shell```에서 쓸 수 있는 몇 가지 코딩 예제를 제시하고, 그것들의 결과를 출력으로 보여줍니다. 
1. 문자열에서 특정 문자 모두 삭제하기
```
string replace --all [-r | --regex] "Pattern" ""
```

예를 들어, 문자열 "hello123bye"에서 숫자를 모두 삭제하려면 다음과 같이 작성합니다.
```
set input "hello123bye"
echo $input | string replace --all -r "[0-9]" ""
```
출력: "hellobye"

2. 파일에서 특정 패턴과 일치하는 줄 삭제하기
```
sed '/Pattern/d' File
```

예를 들어, "file.txt"에서 "hello"가 포함된 줄을 모두 삭제하려면 다음과 같이 작성합니다.
```
sed '/hello/d' file.txt
```
출력: "goodbye"

## 깊게 파고들기
문자열에서 특정 패턴과 일치하는 문자를 삭제하는 것은 데이터 정제나 파일 정리에 매우 유용합니다. ```Fish Shell```을 사용하면 간단한 명령어로 이 작업을 쉽게 수행할 수 있습니다. 정규식을 사용하면 더 복잡한 패턴도 일치시킬 수 있으며, 여러 줄의 파일에서도 적용할 수 있습니다. 하지만 주의할 점은 이 작업이 데이터를 정제하는 용도로만 사용되어야 한다는 것입니다. 실제 데이터를 삭제하는 것보다 정제되지 않은 데이터를 백업해두는 것이 좋습니다.

# 관련 자료
- [Fish Shell 공식 홈페이지](https://fishshell.com/)
- [Fish Shell 시작하기 튜토리얼](https://fishshell.com/docs/current/tutorial.html)
- [Fish Shell 공식 GitHub 페이지](https://github.com/fish-shell/fish-shell)