---
title:                "Gleam: 정규식 사용하기"
simple_title:         "정규식 사용하기"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/using-regular-expressions.md"
---

{{< edit_this_page >}}

# 왜?

정규식을 사용하는 것은 코드를 간결하게 유지하고 원하는 패턴을 찾는 데 매우 유용합니다.

## 어떻게?

```Gleam
let input_string = "Hello, world!";
let regex = regex!("[a-zA-Z]+");
let matches = regex.matches(input_string);
```

위의 코드는 "Hello, world!"라는 문자열에서 알파벳 문자만 매칭하는 간단한 예제를 보여줍니다.

```Gleam
io.format(
    ~s = "매칭된 문자열: {:?}",
    regex.match(input_string),
)
```

위의 코드는 "매칭된 문자열: Hello"라는 출력을 생성합니다.

## Deep Dive

정규식을 작성할 때 몇 가지 작성 방법이 있습니다. 첫 번째로는 "[a-zA-Z]+"와 같이 문자 그룹을 사용하는 것입니다. 또한 "Hello"라는 문자열에서 문자 "e"를 찾는 대신 "[^a-x]+"와 같이 부정 문자 그룹을 사용해서 찾을 수도 있습니다.

정규식 패턴 뒤에는 매칭 옵션을 추가할 수도 있는데, 가장 일반적인 옵션은 "g"입니다. 이 옵션을 사용하면 매칭된 모든 결과를 찾게 됩니다.

## See Also

- [Gleam 공식 웹사이트](https://gleam.run/)
- [Gleam 공식 문서](https://gleam.run/documentation/)