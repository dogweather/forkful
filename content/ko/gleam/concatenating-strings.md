---
title:                "Gleam: 문자열 연결"
programming_language: "Gleam"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/concatenating-strings.md"
---

{{< edit_this_page >}}

# 왜
이 글에서는 왜 누군가 문자열을 연결하는 것에 참여해야 하는지에 대해 알아보겠습니다. 

많은 프로그래밍 언어에서 문자열을 조합하는 것은 매우 자주 사용되는 작업입니다. 문자열을 연결하는 것은 단순하지만, 이를 제대로 이해하고 활용한다면 코드를 효율적으로 작성할 수 있습니다. 

# 어떻게
이제 한글로 된 예제를 통해 문자열을 연결하는 방법을 알아보겠습니다. 아래의 코드 블록에서는 Gleam을 사용하여 문자열을 연결하는 방법을 보여줍니다. 

```Gleam
let language = "Gleam"
let greeting = "안녕하세요?"

let welcome_message = "여러분, " ++ greeting ++ " 저희 " ++ language ++ " 세상에 오신 걸 환영합니다!"
```

위의 코드를 실행하면 다음과 같은 출력이 나옵니다. 

```
여러분, 안녕하세요? 저희 Gleam 세상에 오신 걸 환영합니다!
```

위의 코드에서는 `++` 연산자를 사용하여 문자열을 연결하였습니다. 이 연산자는 양쪽의 문자열을 하나로 합쳐줍니다. 또한 필요한 경우 변수를 사용하여 문자열을 만들 수도 있습니다. 

# 깊게 파보기
문자열을 연결하는 것은 매우 간단한 작업이지만, 다양한 방식으로 이를 활용할 수 있습니다. 예를 들어, 여러 개의 변수를 사용하거나 조건문과 함께 사용하는 등 다양한 상황에서 문자열을 연결하는 방법이 다를 수 있습니다. 

또한 문자열 연결은 문자열 이외의 자료형을 연결하는 것도 가능합니다. 예를 들어, `1`과 `2`라는 숫자를 문자열로 변환한 다음에 연결하면 결과는 `"12"`가 됩니다. 따라서 문자열을 연결하는 방법을 이해하면 다양한 자료형을 조합하여 원하는 결과를 얻을 수 있습니다. 

# 또 다른 글들
- [Gleam 공식 문서 - 문자열 사용하기](https://gleam.run/documentation/guide/strings.html)
- [Gleam 공식 예제 - 문자열 연결하기](https://gleam.run/documentation/getting-started/concatenating-strings.html)
- [Gleam 포럼 - 문자열 연결에 관한 토론](https://gleam.run/discuss/t/concatenation-in-gleam/294) 

# 참고
Gleam 공식 문서 및 예제, 포럼 등을 참고하시면 더 많은 정보를 얻을 수 있습니다. 문자열 연결은 매우 간단하지만, 다양한 방식으로 활용할 수 있으니 꼭 익혀두시기 바랍니다.