---
title:                "컴퓨터 프로그래밍에서 명령줄 인수 읽기"
html_title:           "Clojure: 컴퓨터 프로그래밍에서 명령줄 인수 읽기"
simple_title:         "컴퓨터 프로그래밍에서 명령줄 인수 읽기"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

뭐, 왜?
==============
우선, 우리는 프로그램을 만들 때 사용자로부터 입력을 받아 프로그램의 실행 결과를 조절하고 싶을 때가 있습니다. 이 때, 콘솔 창에서 받은 입력을 읽어들이는 것이 필요합니다. 이를 읽기 위해, 우리는 커맨드 라인 인자를 읽어야 합니다. 이를 통해 프로그램은 사용자로부터 입력 값을 받아 실행 결과를 조절할 수 있게 됩니다.

어떻게?
==============
우리는 클로저에서 ```(command-line-args)```를 호출하는 것으로 커맨드 라인 인자를 읽을 수 있습니다. 다음은 간단한 예제입니다.

```Clojure
(def args (command-line-args))
(println "안녕하세요, " (first args) "님!")
```

위 코드를 실행하면 "안녕하세요, [사용자이름] 님!"이라는 출력 결과를 볼 수 있습니다.

더 깊이 들어가보면?
==============
우리가 커맨드 라인 인자를 읽는 방법은 실제로 오래된 역사를 가지고 있습니다. 초기에는 명령어로 실행되는 프로그램들은 사용자의 입력 값이 필요 없이 실행되었습니다. 하지만, 사용자와 상호작용 할 필요가 있는 프로그램들이 더 많아지면서, 커맨드 라인 인자를 읽는 기능이 추가되었습니다. 클로저를 비롯한 많은 프로그래밍 언어들에서도 커맨드 라인 인자를 읽는 기능이 제공됩니다.

또한, 다른 방법으로는 환경 변수를 사용하는 것이 있습니다. 이 경우에는 사용자가 직접 값을 설정해주어야 합니다.

이를 구현하는 방법은 운영 체제 별로 다를 수 있습니다. 우리가 작성하는 프로그램이 실행되는 운영 체제를 확인하고, 이를 기반으로 적절한 방법을 선택해야 합니다.

참조
==============
관련 정보를 더 알고 싶다면, 다음 링크들을 확인해보세요.
- [Official Clojure Documentation for Command Line Arguments](https://clojure.org/reference/jvm_commandline#_command_line_arguments)
- [ClojureDocs for command-line-args](https://clojuredocs.org/clojure.core/command-line-args)
- [A blog post on using command line arguments in Clojure](https://dzone.com/articles/handling-command-line-arguments-in-clojure)