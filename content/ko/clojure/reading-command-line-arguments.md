---
title:                "Clojure: 명령줄 인수 읽기"
simple_title:         "명령줄 인수 읽기"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 왜

커맨드 라인 인수를 읽는 것은 프로그래머에게 매우 유용합니다. 인수를 읽는 능력은 여러분이 쉘 스크립트를 작성하거나 Clojure 애플리케이션을 만들 때 매우 중요한 요소입니다.

## 어떻게

커맨드 라인 인수를 읽기 위해서는 Clojure의 `command-line-args` 함수를 사용하면 됩니다. 이 함수는 현재 프로세스의 커맨드 라인 인수를 가져옵니다. 예를 들어, `lein run arg1 arg2` 명령으로 프로그램을 실행하면, `command-line-args` 함수는 `["arg1" "arg2"]`를 반환합니다.

```Clojure
(defn -main [& args]
  (let [command-line-args (command-line-args)]
    (println command-line-args)))

; Output: ["arg1" "arg2"]
```

## 깊게 파고들기

`command-line-args` 함수는 기본적으로 스트링 형태의 인수만을 반환합니다. 그러나 만약에 여러분이 숫자를 인수로 받고 싶다고 가정해 봅시다. 이 경우에는 `parse-int` Clojure 함수를 사용하여 숫자로 변환할 수 있습니다. 또한 여러분이 다양한 인수를 받아서 사용할 때 `getopts` 라이브러리를 사용하는 것도 좋은 옵션입니다.

## See Also

- `command-line-args` 함수: https://clojuredocs.org/clojure.core/command-line-args
- `parse-int` 함수: https://clojuredocs.org/clojure.core/parse-int
- `getopts` 라이브러리: https://github.com/clojure/tools.cli