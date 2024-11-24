s4_to_list <- function(s4_object) {
  if (!isS4(s4_object)) stop("The input must be an S4 object")
  slot_names <- slotNames(s4_object)
  slots_as_list <- lapply(slot_names, function(slot_name) {
    slot(s4_object, slot_name)
  })
  names(slots_as_list) <- slot_names
  slots_as_list
}
