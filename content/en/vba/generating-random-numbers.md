---
title:                "Generating random numbers"
date:                  2024-02-01T13:31:39.852046-07:00
model:                 gpt-4-0125-preview
simple_title:         "Generating random numbers"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/vba/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?

Random numbers in programming are like the seasoning in cooking – they add flavor and unpredictability. In Visual Basic for Applications (VBA), programmers use random numbers for tasks like simulating scenarios, generating test data, or deciding which part of the code to execute next.

## How to:

Generating a random number in VBA might seem like waving a magic wand, but it's actually pretty straightforward. Before you cast any spells, you'll need to summon the `Randomize` statement; it initializes the random number generator. Without it, your "random" numbers might not feel so random because they could start from the same seed value each time you run your code.

Here's a simple incantation to generate a random number between 1 and 100:

```basic
Sub GenerateRandomNumber()
    ' Initialize the random number generator to make results unpredictable
    Randomize
    
    ' Generate a random number between 1 and 100
    Dim myRandomNumber As Integer
    myRandomNumber = Int((100 * Rnd()) + 1)
    
    ' Display the random number
    MsgBox "Your random number is: " & myRandomNumber
End Sub
```

When you run this spell, a message box will pop up, revealing a number that fate has chosen for you.

## Deep Dive

The mechanics of generating random numbers in VBA hinge on the `Rnd` function, coupled with the `Randomize` statement. Without `Randomize`, `Rnd` generates the same sequence of numbers every time your program is run, because it's based on a deterministic algorithm. Invoking `Randomize` sets a new starting point for `Rnd`, based on the system clock, making the sequence appear random.

Historically, the pseudo-random number generators (PRNG), like the one used in VBA, have been sufficient for most applications not requiring high levels of randomness. However, for more security-sensitive applications, such as cryptography, these generators fall short. In those cases, cryptographically secure PRNGs (CSPRNGs) are preferred due to their improved unpredictability. VBA's generator is fine for general use, simulations, and games, but developers working on more critical applications should look towards more robust alternatives, possibly integrating external libraries capable of delivering the necessary level of randomness.
