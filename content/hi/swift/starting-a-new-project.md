---
title:                "नए परियोजना की शुरुआत"
html_title:           "Swift: नए परियोजना की शुरुआत"
simple_title:         "नए परियोजना की शुरुआत"
programming_language: "Swift"
category:             "Swift"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/swift/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Kyun

Kisi bhi naye project ko shuru karne ke peeche kya hai? Ye ek common sawal hai jo har coder ke dimaag mein hota hai. Ye ek challenging aur exciting task hai, jisme aap apne creativity aur coding skills ko test kar sakte hain. Iske alawa, naye project shuru karne se aap apne portfolio ko bhi enhance kar sakte hain aur future mein job opportunities bhi badh sakti hain.

## Kaise

Agar aap ek naya project shuru karna chahte hain Swift mein, toh yeh steps follow kare:

1. Xcode ko open kare aur "Create a new Xcode project" par click kare
2. Next, aapko project ka naam dena hoga, aur uske baad "Swift" ko select kare
3. Project save karne ke liye ek folder choose kare
4. "Main.storyboard" aur "ViewController.swift" files automatically create ho jayenge
5. "Main.storyboard" mein UI elements jaise buttons, labels, text fields aur images ko drag and drop karke design kare
6. "ViewController.swift" file mein aapka code likhe jisse UI elements ke saath interaction ho sake
7. "Run" button par click karke apna project run kare aur dekhe kaise output aata hai.

```Swift 
// Sample code for creating a button and setting its title
let myButton = UIButton()
myButton.frame = CGRect(x: 50, y: 50, width: 100, height: 50)  // sets button's position and size
myButton.setTitle("Click Here", for: .normal) // sets button's title text
myButton.addTarget(self, action: #selector(buttonClicked), for: .touchUpInside) // adds action to button
self.view.addSubview(myButton) // adds button to the view

@objc func buttonClicked() {  // function to be executed when button is clicked
    // insert code here
}
```

## Deep Dive

1. Swift mein project shuru karna kaafi aasan hai, par aapko ek acchi foundation chahiye. Programming concepts aur Swift syntax ko acche se samajhne ke liye, aap tutorials aur online resources ka istemal kar sakte hain.
2. UI design ke liye, aap Xcode ka interface builder tool use kar sakte hain, jisme aap drag and drop se badiya UI banane mein madad milegi.
3. Swift mein loops, conditionals, arrays, functions, classes, aur structures jaise concepts ko acche se samajhna project ke liye kaafi zaruri hai.
4. Project ko organized rakhne ke liye aap files ko groups mein divide kar sakte hain aur proper file naming conventions use kar sakte hain.
5. Project ke development ke saath saath testing aur debugging bhi equally important hai. Xcode mein available debugging tools ki madad se aap errors ko easily identify aur fix kar sakte hain.

## Dekhiye Bhi

1. [Swift Programming Language Guide in Hindi](https://www.simplifiedios.net/swift-tutorials-in-hindi/)
2. [Official Apple Documentation for Swift](https://developer.apple.com/swift/)
3. [Online Swift Tutorials in Hindi](https://www.youtube.com/watch?v=3p7s7-KJ4hU&list=PLZe3ZU0W5JtO6jP5jW7dYcWnhap7o5D_g)