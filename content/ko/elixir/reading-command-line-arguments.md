---
title:    "Elixir: 컴퓨터 프로그래밍에서 커맨드 라인 인수 읽기"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## 왜
커맨드 라인 인자를 읽는 방법에 대해 배우고 싶은 사람들을 위해 이 블로그 포스트가 제공되었습니다.

## 어떻게
커맨드 라인 인자를 읽는 것은 Elixir 프로그래밍에서 중요한 기술입니다. 다음 예제를 따라해보세요.

```Elixir
args = System.argv()
IO.puts "입력받은 인자는 #{inspect args} 입니다."
```

위 코드를 실행하면 다음과 같은 결과를 볼 수 있습니다.

```
elixir script.exs arg1 arg2 arg3
입력받은 인자는 ["arg1", "arg2", "arg3"] 입니다.
```

위와 같은 방식으로 입력받은 인자를 리스트로 저장할 수 있습니다. 이제 이를 활용하여 원하는 로직을 구현할 수 있습니다.

## 더 알아보기
커맨드 라인 인자를 읽는 것은 웹 애플리케이션을 개발할 때 유용합니다. 예를 들어, 패스워드나 포트 번호 등 중요한 정보를 커맨드 라인 인자로 전달받아서 사용할 수 있습니다. 이를 통해 보안에 더욱 신경 쓸 수 있습니다.

## 관련 링크
[공식 Elixir 문서: System.argv/0](https://hexdocs.pm/elixir/System.html#argv/0)

## 더 알아보기
[Elixir를 이용한 웹 개발](https://wiki.korea.ac.kr/pages/viewpage.action?pageId=60671705)

[커맨드 라인 인자를 활용한 보안 측면](https://wayhome25.github.io/security/2017/03/07/elgg_%EC%9D%B4%EB%AF%B8%EC%A7%80_%EC%B9%98%EB%A3%8C%EC%9D%B8%EC%9D%98_%EB%B3%B4%EC%95%88%EC%B8%A1%EB%B3%80_%EC%BB%A4%EB%A7%A8%EB%93%9C_%EB%9D%BD%EA%B8%B0/)