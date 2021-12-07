- ~~Implement validateEven() to return true/false depending on where the argument is even~~
- ~~implement validateOdd(), validatePositive, validateNegative with same interface~~
- ~~implement validateEvenAndNegative. In this case, how do we know which condition failed? Refactor to introduce a "Failure Reason", which indicates which condition failed. If both conditions failed, then the failure has 2 failure reasons~~
- ~~implement validatePositiveOrZero(), with the same comments as in the previous step.~~
  - (hint: this can only fail with 2 reasons, because both conditions must fail, due to "or")
- ~~implement validateEvenAndPositiveOrZero(). For example: 0, 2, 4 all pass, but 1, 3, 5 all fail. Also, -2 fails (it is even, but it is not positive-or-zero)~~
- ~~remove duplication before it's too much~~
- ~~write validations for strings or some data type. Start from single condition, then combine two conditions, then try to build bigger combinations of coditions.~~
- ~~rename validateEvenWithErrorMessage to validateEven again~~ 
- ~~extract ValidationInput interface~~

## Hints
- ~~when you're factoring, think about the composite design pattern or the specification pattern~~
- incorporate the advice of "Alexis King's article" [Parse, Don't Validate](https://lexi-lambda.github.io/blog/2019/11/05/parse-don-t-validate/) into the validator
- ~~try not to use Specific pattern implementation, but do pay attention carefully to duplication as it emerges. Where does the design try to go?~~

## Questions
- what's the difference between |> and <| in fsharp?
- how to aggregate validationresult?
- how to not validationresult?
- when to add paren around function/type?
- how to flatten list of list?
