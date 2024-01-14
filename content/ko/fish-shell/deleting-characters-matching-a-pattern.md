---
title:                "Fish Shell: 패턴과 일치하는 문자 삭제"
simple_title:         "패턴과 일치하는 문자 삭제"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# 왜

때로는 문자열에서 특정 패턴과 일치하는 문자를 삭제해야 할 때가 있습니다. 이럴 때는 어떻게 해야 할지 알아보겠습니다.

## 어떻게

```Fish Shell```을 사용하여 문자열에서 특정 패턴과 일치하는 문자를 삭제하는 방법은 아주 간단합니다. 먼저 ```string``` 명령어로 문자열을 지정하고, ```sed``` 명령어를 사용하여 해당 패턴을 삭제하는 ```s``` 명령어를 추가합니다. 마지막으로 ```echo``` 명령어를 사용하여 삭제된 문자열을 출력합니다. 아래는 실제 코드와 출력 예시입니다.

```Fish Shell
# 문자열 지정하기
set str "hello world!"

# 문자열에서 "o" 문자 삭제하기
echo $str | sed 's/o//g' 
# 출력: hell wrld!
```

위의 예시에서는 ```"o"```라는 패턴에 일치하는 문자가 모두 삭제되었습니다. 만약 원하는 패턴이 아니라면, ```s``` 명령어 안에 다른 정규표현식을 사용하여 패턴을 지정할 수 있습니다.

## 딥 다이브

일반적으로 문자열에서 특정 패턴과 일치하는 문자를 삭제하려면 정규표현식을 사용합니다. 정규표현식은 패턴 매칭을 위한 패턴의 집합으로, 간단한 패턴부터 복잡한 패턴까지 다양한 경우에 사용할 수 있습니다. 정규표현식을 자세히 배우고 응용하면 문자열을 효율적으로 다룰 수 있으며, 문자열 삭제 외에도 다양한 작업에 응용할 수 있습니다.

# 참고 자료

- [Fish Shell 공식 사이트](https://fishshell.com/)
- [정규표현식 간단 정리](https://wikidocs.net/1669)
- [sed 명령어 자세한 사용 방법](http://www.cubicrace.com/2016/03/efficient-pattern-searching-using-sed.html)