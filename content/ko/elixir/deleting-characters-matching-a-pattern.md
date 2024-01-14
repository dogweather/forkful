---
title:                "Elixir: 패턴과 일치하는 문자 삭제하기"
simple_title:         "패턴과 일치하는 문자 삭제하기"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# 왜

이 글에서, 우리는 엘릭서 프로그래밍 언어에서 문자열 패턴을 지우는 방법을 살펴보겠습니다. 이 작업을 왜 하는지에 대해서 간단히 알아보겠습니다.

# 방법

우리는 우선 문자열 내에서 원하는 패턴을 검색하고, 그 패턴과 일치하는 문자를 모두 지우는 함수를 만들어볼 것입니다. 이를 위해 `Regex` 모듈을 사용할 것입니다.

```Elixir
# 패턴과 일치하는 문자를 지우는 함수
def delete_pattern(string, pattern) do
  # 패턴과 일치하는 문자를 검색
  matches = Regex.scan(%r/#{pattern}/, string)
  # 일치하는 문자를 지움
  string |> String.replace_pattern(%r/#{pattern}/, "")
end

# 예시
puts delete_pattern("Hello World!", "l") # Heo Word!
```

# 깊게 살펴보기

`Regex` 모듈을 사용하여 문자열 내에서 패턴을 지우는 방법을 살펴보았습니다. 이를 좀 더 깊게 들어가보겠습니다.

먼저 `Regex.scan` 함수는 두 개의 인자를 받습니다. 첫 번째 인자는 패턴을 나타내는 `regex`이고, 두 번째 인자는 검색할 문자열입니다. 이 함수는 일치하는 패턴이 발견되면 해당 패턴을 리스트로 반환합니다. 따라서 `matches`변수에 일치하는 패턴을 저장하게 됩니다.

다음으로 `String.replace_pattern` 함수는 세 개의 인자를 받습니다. 첫 번째 인자는 문자열 자체이고, 두 번째 인자는 바꿀 패턴을 나타내는 `regex`이고, 세 번째 인자는 새로 바꿀 문자열입니다. 이 함수는 첫 번째 인자인 문자열에서 두 번째 인자인 패턴과 일치하는 문자를 세 번째 인자인 문자열로 바꿔줍니다.

따라서 위 예시에서는 `"Hello World!"`라는 문자열에서 `"l"`이라는 패턴과 일치하는 모든 문자를 `""`로 바꿔주었습니다. 따라서 출력 결과는 "eHoo Word!"가 됩니다.

# 참고 문서

- [Elixir Regex 모듈 공식 문서](https://hexdocs.pm/elixir/Regex.html)
- [Mastering Elixir Regex - Elixir virtuoso](https://blog.openstrapping.org/2017/09/08/mastering-elixir-regex/)