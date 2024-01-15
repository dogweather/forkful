---
title:                "파싱 HTML"
html_title:           "Elixir: 파싱 HTML"
simple_title:         "파싱 HTML"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/parsing-html.md"
---

{{< edit_this_page >}}

## 왜

HTML 파싱에 대해 관심이 있는 분들은 웹 스크래핑, 데이터 마이닝, 자동화 작업 등 다양한 목적으로 HTML을 파싱할 수 있습니다. Elixir는 강력한 패턴 매칭을 제공하기 때문에 이러한 작업에 이상적입니다. 

## 방법

Elixir에서 HTML 파싱을 하는 가장 일반적인 방법은 Floki 라이브러리를 사용하는 것입니다. Floki는 CSS 선택자를 사용하여 HTML을 쉽게 파싱할 수 있는 강력한 도구입니다. 아래는 Floki를 사용하여 제목 태그를 추출하는 예제 코드입니다.

```elixir
# Floki 라이브러리를 임포트합니다.
import Floki

# 웹페이지의 HTML을 가져옵니다. 
html = "<div><h1>Hello, World!</h1></div>"

# Floki를 사용하여 제목 태그를 추출합니다.
title_tag = Floki.find(html, "h1") |> Floki.text

# 결과를 출력합니다.
IO.puts title_tag
```

출력 결과는 다음과 같습니다.

```
Hello, World!
```

선택자를 사용하면 더욱 복잡한 HTML 구조에서도 원하는 요소를 파싱할 수 있습니다. 다음은 클래스가 "product"인 제품의 가격을 추출하는 예제 코드입니다.

```elixir
# Floki 라이브러리를 임포트합니다.
import Floki

# 웹페이지의 HTML을 가져옵니다. 
html = """
<div class="product">
  <h2>iPhone 12</h2>
  <p class="price">$999</p>
</div>
"""

# Floki를 사용하여 가격 태그를 추출합니다.
price_tag = Floki.find(html, "div.product p.price") |> Floki.text

# 결과를 출력합니다.
IO.puts price_tag
```

출력 결과는 다음과 같습니다.

```
$999
```

더 많은 코드 예제와 결과는 [Floki 공식 문서](https://hexdocs.pm/floki/readme.html)에서 확인할 수 있습니다.

## 깊게 파헤치기

Elixir의 최신 버전은 HTML 파싱에 필요한 모든 기능을 제공합니다. Floki 라이브러리를 사용하면 강력한 패턴 매칭과 CSS 선택자를 이용하여 HTML을 쉽게 파싱할 수 있습니다. 또한 Elixir는 멀티 스레드를 지원하기 때문에 HTML을 동시에 파싱할 수 있어서 빠른 속도로 데이터를 수집할 수 있습니다.

## 또 다른 참고 자료

- [Floki 공식 문서](https://hexdocs.pm/floki/readme.html)
- [Elixir 공식 웹사이트](https://elixir-lang.org/)
- [Floki를 사용한 HTML 파싱 예제](https://medium.com/@akoutmos/getting-started-with-floki-elixirs-best-html-parser-a8893927c0b0)