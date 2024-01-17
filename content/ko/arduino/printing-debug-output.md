---
title:                "디버깅 출력하기"
html_title:           "Arduino: 디버깅 출력하기"
simple_title:         "디버깅 출력하기"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/printing-debug-output.md"
---

{{< edit_this_page >}}

# 뭐고 왜? 
디버그 출력(printing debug output)이란 무엇일까요? 이는 프로그래머들이 디버깅을 하는 동안 프로그램이 어떻게 동작하고 있는지를 확인하기 위해 사용되는 작업입니다. 디버그 출력은 에러를 추적하고 문제를 해결하는 데 매우 유용합니다. 

# 어떻게: 
디버그 출력은 아두이노(Arduino)를 사용하여 매우 간단하게 수행할 수 있습니다. 아래 코드 블록을 참조하십시오. 

```Arduino

// 디버그 출력을 위한 시리얼 모니터를 설정합니다.
Serial.begin(9600);

// 디버그 출력을 하기 위해 "Hello World!" 메시지를 보냅니다.
Serial.println("Hello World!");

```

위의 코드는 아두이노 보드에 연결된 시리얼 모니터를 사용하여 "Hello World!" 메시지를 출력하는 예제입니다. 메시지는 연결된 컴퓨터의 시리얼 포트로 전송되어 콘솔에 표시됩니다. 

# 더 깊게: 
디버그 출력은 프로그래밍의 초창기부터 사용되어 왔습니다. 예전에는 프로그래머들이 디버그 시 메모리에 있는 변수 값을 확인하기 위해 물리적인 디바이스에 연결된 LED 등을 사용했습니다. 하지만 현재는 시리얼 모니터와 같은 디버그 도구를 사용하여 더욱 편리하게 디버깅을 할 수 있습니다. 또한 특정 조건에서만 디버그 출력을 활성화하거나 디버그 메시지의 레벨을 조절하는 등 다양한 방법으로 디버그 출력을 사용할 수 있습니다.

# 더 많은 정보: 
더 많은 정보를 원하시면 아래 링크를 참조하십시오. 

- https://learn.sparkfun.com/tutorials/serial-communication 현재 코드는 아두이노 IDE와 호환되도록 작성되었습니다.