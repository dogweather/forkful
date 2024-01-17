---
title:                "새로운 프로젝트 시작하기"
html_title:           "Ruby: 새로운 프로젝트 시작하기"
simple_title:         "새로운 프로젝트 시작하기"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/starting-a-new-project.md"
---

{{< edit_this_page >}}

## 무엇 및 왜? 
새로운 프로젝트를 시작한다는 것은 무슨 뜻일까요? 프로그래머들이 왜 그것을 하는 걸까요? 프로젝트를 시작한다는 것은 새로운 아이디어를 구상하고 그것을 현실로 만드는 과정입니다. 프로그래머들은 새로운 기술을 배우고, 문제를 해결하는 방법을 찾고, 창의력을 발휘하는 데에 있어서 새로운 프로젝트를 시작하는 것을 좋아합니다. 

## 방법: 
```Ruby
# 새로운 프로젝트 만들기 
puts "새로운 프로젝트를 시작합니다!"

# 프로젝트 이름 지정하기 
project_name = "나의 첫번째 프로젝트"

puts "#{project_name} 만들기 시작!"

# 라이브러리 추가하기 
require 'rubygems'

# 프로젝트 시작하기 
project_start = Time.now 

puts "프로젝트를 시작합니다.."

# 프로젝트의 목적 설정하기 
purpose = "새로운 아이디어를 실현시키기 위한 것"

# 프로젝트 목적 출력하기 
puts "프로젝트의 목적은 #{purpose} 입니다."

# 프로젝트 완료 시간 계산하기 
project_complete = Time.now + 86400 

# 프로젝트의 완료 시간 출력하기 
puts "프로젝트 마감일은 #{project_complete} 입니다. "

# 프로젝트 완료하기
puts "프로젝트가 완료되었습니다."
```

결과:
새로운 프로젝트를 시작합니다!
나의 첫번째 프로젝트 만들기 시작!
프로젝트를 시작합니다..
프로젝트의 목적은 새로운 아이디어를 실현시키기 위한 것입니다.
프로젝트 마감일은 2019-11-05 13:16:21 +0900 입니다.
프로젝트가 완료되었습니다.

## 깊이 파고들기: 
프로젝트를 시작하는 것은 프로그래밍에서 매우 중요한 과정입니다. 프로젝트를 시작함으로써 새로운 기술을 배울 수 있고, 문제를 해결하는 방법을 찾을 수 있습니다. 또한, 여러분의 창의력을 발휘할 수 있는 기회가 됩니다. 프로젝트를 시작하는 다른 방법에는 프로젝트 생성기를 사용하기도 합니다. 이 방식은 프로젝트에 필요한 시작 코드를 자동으로 만들어 줍니다. 또 다른 대안은 오픈소스 프로젝트에 참여하는 것입니다. 오픈소스 프로젝트에 참여하면 다른 개발자들과 함께 협업하며 새로운 기술을 배우는 기회를 가질 수 있습니다. 프로젝트를 시작할 때 가장 중요한 것은 목적을 정하는 것입니다. 목적을 정하면 프로젝트의 방향성을 명확하게 설정할 수 있으며 완성까지의 계획을 세우는 데에도 도움이 됩니다.

## 관련 자료:
- Ruby 공식 홈페이지: https://www.ruby-lang.org/ko/
- Ruby 프로젝트 튜토리얼: https://ruby-doc.org/core-2.6.3/doc/pickaxe-2.html
- 오픈소스 프로젝트 참여 방법: https://opensource.guide/ko/
- 프로젝트 생성기: https://github.com/RailsApps/rails-composer