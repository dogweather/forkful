---
title:    "Elixir: 정규식 사용하기"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/using-regular-expressions.md"
---

{{< edit_this_page >}}

# 왜?

정규 표현식은 코드에서 특정한 패턴을 찾고, 추출하거나 변경하는 데 아주 유용합니다. 이를 통해 코드를 더 간결하고 효율적으로 작성할 수 있으며, 데이터 처리 작업을 더 빠르고 쉽게 수행할 수 있습니다.

## 어떻게 하나요?

정규 표현식을 사용하려면 Elixir에서 `Regex` 모듈을 import해야 합니다. 그리고 `~r` 키워드를 사용하여 정규 표현식을 만들 수 있습니다. 예를 들어, 다음과 같은 코드로 이메일 주소에서 사용자 이름을 추출할 수 있습니다.

```Elixir
import Regex

email_address = "john.doe@example.com"
regex = ~r/[a-z]+\.([a-z]+)@[a-z]+\.[a-z]+/
match = Regex.run(regex, email_address)
IO.puts match[1] # output: "john"
```

## 깊은 공부

정규 표현식에서 사용되는 패턴과 메타 문자의 의미를 정확히 알아야 합니다. 그리고 직접 표현식을 작성하면서 연습하는 것이 가장 중요합니다. 또한 Elixir에서는 부분 일치를 지원하는 메서드들도 있으므로 사용 가능성도 살펴볼 필요가 있습니다.

아래는 자주 사용되는 정규 표현식 패턴과 관련된 링크들입니다.

# 관련 자료

- [Elixir 정규 표현식 문서](https://hexdocs.pm/elixir/Regex.html)
- [정규 표현식 테스트 사이트](https://regex101.com/)
- [정규 표현식 치트시트](https://www.rexegg.com/regex-quickstart.html)